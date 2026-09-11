(** Immutable, schema-checked core representation for B2T2 tables. *)

type sort =
  | String_sort
  | Int_sort
  | Float_sort
  | Bool_sort
  | Sequence_sort of sort
  | Table_sort (* shape of table could change to check this recursively as well *)
  | Unknown_sort

type value =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null (* missing val *)
  | Sequence of value list
  | Nested_table of table

and column = {
  name: string;
  sort: sort;
}

and row = (string * value) list

and table = {
  schema: column list;
  rows: row list;   
 }

(*
  (* Alt. Consideration 1: *)
  (* table that store rows as a polymorphic type, but OCaml could not easily connect schema to object's fields. *)
  let hs: header = ["name"; "age"; "quiz1"]
  let g_book =
      object
        val mutable name = ""
        val mutable age = 0
        val mutable quiz1 = 0.0

        method name n = name <- n
        method age a = age <- a
        method quiz1 s = quiz1 <- s
      end;;

  let gradebook = new table hs;;
  gradebook#add_rows [g_book]

*)

type 'a checked = ('a, string) result

let header (table: table) =
  List.map (fun col -> col.name) table.schema

let schema_names schema =
  List.map (fun col -> col.name) schema

let empty_table : table = { schema = []; rows = [] }

let nrows t = List.length t.rows
let ncols t = List.length t.schema

let check_sort (sort : sort) (value : value) : bool =
    match sort, value with
    | _, Null -> true
    | String_sort, String _ -> true
    | Int_sort, Int _ -> true
    | Float_sort, Float _ -> true
    | Bool_sort, Bool _ -> true
    | Sequence_sort _, Sequence _ -> true
    | Table_sort, Nested_table _ -> true
    | Unknown_sort, _ -> true
    | _ -> false

let find_column table name : column option =
  List.find_opt (fun col -> col.name = name) table.schema
  
let find_in_row column row = List.assoc_opt column row

let get_column t name = 
  if Option.is_none (find_column t name) 
    then 
      Error ("unknown column: " ^ name) 
    else 
      Ok (List.map (fun r -> Option.get (find_in_row name r)) t.rows)

let row t index = 
  if index < 0 then 
    Error "row index must be non-negative" 
  else match List.nth_opt t.rows index with 
  | Some r -> Ok r 
  | None -> Error "row index is outside the table"


let ( let* ) = Result.bind

let select_rows t indices =
  let rec loop acc = function 
  | [] -> Ok { t with rows = List.rev acc } 
  | i :: is -> 
    let* r = 
      row t i in loop (r :: acc) is 
  in 
  loop [] indices
  
let unique values =
  let rec loop seen = function
  | [] -> true
  | value :: rest ->
    if List.mem value seen then
      false
    else
      loop (value :: seen) rest
  in
  loop [] values

let select_columns_3 t columns =
  if not (unique columns) then 
    Error "columns cannot contain duplicates"
  else if not (List.for_all (fun c -> Option.is_some (find_column t c)) columns) then 
    Error "cannot select an unknown column" 
  else 
    Ok { schema = List.map (fun c -> Option.get (find_column t c)) columns; 
          rows = List.map (fun r -> List.map (fun c -> c, Option.get (find_in_row c r)) columns) t.rows }

let drop_columns t col_names = 
  if not (List.for_all (fun c -> Option.is_some (find_column t c))
      col_names)
    then
      Error "cannot drop an unknown column"
  else 
    select_columns_3 t (List.filter (fun c -> not (List.mem c col_names)) (header t))

let value t index column =
  let* r = row t index in
  match find_in_row column r with
  | Some v -> Ok v
  | None -> Error ("unknown column: " ^ column)

let add_column t name values =
  if Option.is_some (find_column t name) then
    Error ("duplicate column: " ^ name)
  else if List.length values <> nrows t then
    Error "a new column needs one value per row"
  else
    Ok { schema = t.schema @ [{ name; sort = Unknown_sort }];
        rows = List.map2 (fun r v -> r @ [name, v]) t.rows values }

let build_column t name f = add_column t name (List.map f t.rows)

let filter t predicate = { t with rows = List.filter predicate t.rows }


(* print helpers *)
let string_of_value = function
  | String x -> Printf.sprintf "%S" x
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Bool x -> string_of_bool x
  | Sequence _ -> "<sequence>"
  | Nested_table _ -> "<table>"
  | Null -> "<missing>"

let string_of_table table =
  let names = header table in
  let cells = List.map (fun row -> List.map (fun name ->
    match List.assoc_opt name row with
    | Some value -> string_of_value value
    | None -> "<missing>"
  ) names) table.rows in
  let widths = List.mapi (fun index name ->
    List.fold_left (fun width row -> max width (String.length (List.nth row index)))
      (String.length name) cells
  ) names in
  let pad width text = text ^ String.make (width - String.length text) ' ' in
  let render row = "| " ^ String.concat " | " (List.map2 pad widths row) ^ " |" in
  let divider = "|-" ^ String.concat "-| -" (List.map (fun width -> String.make width '-') widths) ^ "-|" in
  String.concat "\n" (render names :: divider :: List.map render cells)

let print_table table =
  print_endline (string_of_table table)

(* other helpers *)
let normalize_row schema row =
  let schema_names = List.map (fun col -> col.name) schema in
  let row_names = List.map fst row in
  if not (unique schema_names) then
    Error "a table schema cannot contain duplicate column names"
  else if not (unique row_names) then
    Error "a row cannot contain a column more than once"
  else if List.length row_names <> List.length schema_names
       || not (List.for_all (fun name -> List.mem name row_names) schema_names)
  then
    Error "a row must contain exactly the table's columns"
  else
    let ordered_row =
      List.map
        (fun col -> (col.name, Option.get (find_in_row col.name row)))
        schema
    in
    if List.for_all2
         (fun col (_, value) -> check_sort col.sort value)
         schema ordered_row
    then
      Ok ordered_row
    else
      Error "a row value does not match its column sort"

let create schema =
  if unique (schema_names schema) then
    Ok { schema; rows = [] }
  else
    Error "a table schema cannot contain duplicate column names"

let of_rows schema rows =
  let* table = create schema in
  let rec loop accepted = function
    | [] -> Ok { table with rows = List.rev accepted }
    | row :: remaining ->
        let* valid_row = normalize_row schema row in
        loop (valid_row :: accepted) remaining
  in
  loop [] rows

(* other API *)
let add_rows t new_rows =
    let* validated = of_rows t.schema new_rows in
    Ok { t with rows = t.rows @ validated.rows }

let remove_duplicates values =
  let rec loop seen kept = function
    | [] -> List.rev kept
    | value :: rest ->
        if List.mem value seen then
          loop seen kept rest
        else
          loop (value :: seen) (value :: kept) rest
  in
  loop [] [] values

let count table column_name =
  let* column =
    match find_column table column_name with
    | Some column -> Ok column
    | None -> Error ("unknown column: " ^ column_name)
  in
  let* values = get_column table column_name in
  let rows =
    List.map
      (fun value ->
        let occurrences = List.length (List.filter (( = ) value) values) in
        ["value", value; "count", Int occurrences])
      (remove_duplicates values)
  in
  of_rows
    [{ name = "value"; sort = column.sort }; { name = "count"; sort = Int_sort }]
    rows

let group_by table output_schema key project aggregate =
  let rec add_to_group group_key projected = function
    | [] -> [group_key, [projected]]
    | (existing_key, projected_values) :: remaining ->
        if existing_key = group_key then
          (existing_key, projected :: projected_values) :: remaining
        else
          (existing_key, projected_values) :: add_to_group group_key projected remaining
  in
  let rec collect groups = function
    | [] -> Ok groups
    | input_row :: remaining ->
        let* group_key = key input_row in
        let* projected = project input_row in
        collect (add_to_group group_key projected groups) remaining
  in
  let rec aggregate_groups rows = function
    | [] -> Ok (List.rev rows)
    | (group_key, projected_values) :: remaining ->
        let* output_row = aggregate group_key (List.rev projected_values) in
        aggregate_groups (output_row :: rows) remaining
  in
  let* groups = collect [] table.rows in
  let* rows = aggregate_groups [] groups in
  of_rows output_schema rows

let group_by_retentive table column_name =
  let* column =
    match find_column table column_name with
    | Some column -> Ok column
    | None -> Error ("unknown column: " ^ column_name)
  in
  let output_schema = [
    { name = "key"; sort = column.sort };
    { name = "groups"; sort = Table_sort };
  ] in
  let key row =
    match find_in_row column_name row with
    | Some value -> Ok value
    | None -> Error ("unknown column: " ^ column_name)
  in
  let aggregate group_key rows =
    Ok ["key", group_key; "groups", Nested_table { table with rows }]
  in
  group_by table output_schema key (fun row -> Ok row) aggregate

let group_by_subtractive table column_name =
  let* column =
    match find_column table column_name with
    | Some column -> Ok column
    | None -> Error ("unknown column: " ^ column_name)
  in
  let output_schema = [
    { name = "key"; sort = column.sort };
    { name = "groups"; sort = Table_sort };
  ] in
  let key row =
    match find_in_row column_name row with
    | Some value -> Ok value
    | None -> Error ("unknown column: " ^ column_name)
  in
  let aggregate group_key rows =
    let retained_group = { table with rows } in
    let* group = drop_columns retained_group [column_name] in
    Ok ["key", group_key; "groups", Nested_table group]
  in
  group_by table output_schema key (fun row -> Ok row) aggregate

let get_value row column_name =
  match find_in_row column_name row with
  | Some value -> Ok value
  | None -> Error ("unknown column: " ^ column_name)

let tfilter = filter

let get_row = row

(*other api definitions -- by chatgpt *)

let rec sort_of_value = function
  | String _ -> String_sort
  | Int _ -> Int_sort
  | Float _ -> Float_sort
  | Bool _ -> Bool_sort
  | Null -> Unknown_sort
  | Sequence values -> Sequence_sort (infer_sort values)
  | Nested_table _ -> Table_sort

and infer_sort values =
  match List.find_opt (fun value -> value <> Null) values with
  | None -> Unknown_sort
  | Some first ->
      let sort = sort_of_value first in
      if List.for_all (check_sort sort) values then sort else Unknown_sort

let require_column table name =
  match find_column table name with
  | Some column -> Ok column
  | None -> Error ("unknown column: " ^ name)

let require_columns table names =
  if not (unique names) then Error "column names cannot contain duplicates"
  else
    let rec loop = function
      | [] -> Ok ()
      | name :: remaining ->
          let* _ = require_column table name in
          loop remaining
    in
    loop names

let is_numeric_sort = function
  | Int_sort | Float_sort -> true
  | _ -> false

let is_categorical_sort = function
  | String_sort | Int_sort | Bool_sort -> true
  | _ -> false

let schema_of_rows rows =
  match rows with
  | [] -> Ok []
  | first :: remaining ->
      let names = List.map fst first in
      if not (unique names) then Error "a row cannot contain duplicate column names"
      else
        let schema = List.map (fun (name, value) -> { name; sort = sort_of_value value }) first in
        let compatible row =
          let row_names = List.map fst row in
          List.length row = List.length first
          && unique row_names
          && List.for_all2 (fun name row_name -> name = row_name) names row_names
          && List.for_all2 (fun column (_, value) -> check_sort column.sort value) schema row
        in
        if List.for_all compatible remaining then Ok schema
        else Error "rows do not have a common schema"

let add_column_unchecked = add_column

let add_column table name values =
  if Option.is_some (find_column table name) then
    Error ("duplicate column: " ^ name)
  else if List.length values <> nrows table then
    Error "a new column needs one value per row"
  else
    let sort = infer_sort values in
    if sort = Unknown_sort && List.exists (fun value -> value <> Null) values then
      Error "a new column must contain values of one sort"
    else
      Ok {
        schema = table.schema @ [{ name; sort }];
        rows = List.map2 (fun row value -> row @ [name, value]) table.rows values;
      }

let build_column table name f =
  add_column table name (List.map f table.rows)

let vcat first second =
  if first.schema <> second.schema then
    Error "vcat requires tables with equal schemas"
  else
    Ok { first with rows = first.rows @ second.rows }

let hcat first second =
  if not (unique (header first @ header second)) then
    Error "hcat requires tables with disjoint column names"
  else if nrows first <> nrows second then
    Error "hcat requires tables with the same number of rows"
  else
    Ok {
      schema = first.schema @ second.schema;
      rows = List.map2 (@) first.rows second.rows;
    }

let values rows =
  if rows = [] then Error "values requires at least one row"
  else
    let* schema = schema_of_rows rows in
    of_rows schema rows

let cross_join first second =
  if not (unique (header first @ header second)) then
    Error "crossJoin requires tables with disjoint column names"
  else
    Ok {
      schema = first.schema @ second.schema;
      rows = List.concat_map (fun left -> List.map (fun right -> left @ right) second.rows) first.rows;
    }

let left_join first second keys =
  let* () = require_columns first keys in
  let* () = require_columns second keys in
  let compatible_keys =
    List.for_all (fun key ->
      match find_column first key, find_column second key with
      | Some left, Some right -> left.sort = right.sort
      | _ -> false) keys
  in
  if not compatible_keys then Error "leftJoin keys must have equal sorts"
  else
    let extra_schema = List.filter (fun column -> not (List.mem column.name keys)) second.schema in
    if not (unique (header first @ List.map (fun column -> column.name) extra_schema)) then
      Error "leftJoin would create duplicate column names"
    else
      let matches left right =
        List.for_all (fun key -> find_in_row key left = find_in_row key right) keys
      in
      let add_extra left =
        match List.find_opt (matches left) second.rows with
        | Some right -> left @ List.filter (fun (name, _) -> not (List.mem name keys)) right
        | None -> left @ List.map (fun column -> column.name, Null) extra_schema
      in
      Ok { schema = first.schema @ extra_schema; rows = List.map add_extra first.rows }

let get_column_at table index =
  if index < 0 then Error "column index must be non-negative"
  else
    match List.nth_opt table.schema index with
    | None -> Error "column index is outside the table"
    | Some column -> get_column table column.name

let select_rows_mask table mask =
  if List.length mask <> nrows table then
    Error "row mask must have one Boolean per table row"
  else
    Ok {
      table with
      rows = List.fold_right2 (fun keep row kept -> if keep then row :: kept else kept) mask table.rows [];
    }

let select_columns_mask table mask =
  if List.length mask <> ncols table then
    Error "column mask must have one Boolean per table column"
  else
    let names =
      List.fold_right2
        (fun keep column kept -> if keep then column.name :: kept else kept)
        mask table.schema []
    in
    select_columns_3 table names

let select_columns_indices table indices =
  if not (unique indices) then Error "column indices cannot contain duplicates"
  else
    let rec names collected = function
      | [] -> Ok (List.rev collected)
      | index :: remaining ->
          if index < 0 then Error "column index must be non-negative"
          else
            match List.nth_opt table.schema index with
            | None -> Error "column index is outside the table"
            | Some column -> names (column.name :: collected) remaining
    in
    let* names = names [] indices in
    select_columns_3 table names

let head table count =
  let total = nrows table in
  let take = if count >= 0 then count else total + count in
  if take < 0 || take >= total then Error "head count is outside the table range"
  else Ok { table with rows = List.filteri (fun index _ -> index < take) table.rows }

let distinct table =
  let rec loop seen kept = function
    | [] -> List.rev kept
    | row :: remaining ->
        if List.mem row seen then loop seen kept remaining
        else loop (row :: seen) (row :: kept) remaining
  in
  Ok { table with rows = loop [] [] table.rows }

let drop_column table name = drop_columns table [name]

let compare_numeric left right =
  match left, right with
  | Int left, Int right -> Some (Int.compare left right)
  | Float left, Float right -> Some (Float.compare left right)
  | _ -> None

let tsort table name ascending =
  let* column = require_column table name in
  if not (is_numeric_sort column.sort) then Error "tsort requires a numeric column"
  else
    let compare_rows left right =
      match find_in_row name left, find_in_row name right with
      | Some left, Some right ->
          (match compare_numeric left right with
           | Some result -> if ascending then result else -result
           | None -> 0)
      | _ -> 0
    in
    Ok { table with rows = List.sort compare_rows table.rows }

let sort_by_columns table names =
  let* () = require_columns table names in
  let* () =
    if List.for_all (fun name ->
      match find_column table name with
      | Some column -> is_numeric_sort column.sort
      | None -> false) names
    then Ok () else Error "sortByColumns requires numeric columns"
  in
  let rec compare_rows left right = function
    | [] -> 0
    | name :: remaining ->
        match find_in_row name left, find_in_row name right with
        | Some left_value, Some right_value ->
            (match compare_numeric left_value right_value with
             | Some 0 -> compare_rows left right remaining
             | Some result -> result
             | None -> 0)
        | _ -> 0
  in
  Ok { table with rows = List.sort (fun left right -> compare_rows left right names) table.rows }

let order_by table comparers =
  let rec compare_rows left right = function
    | [] -> 0
    | (get_key, less_or_equal) :: remaining ->
        let left_key = get_key left in
        let right_key = get_key right in
        if less_or_equal left_key right_key && less_or_equal right_key left_key then
          compare_rows left right remaining
        else if less_or_equal left_key right_key then -1 else 1
  in
  Ok { table with rows = List.sort (fun left right -> compare_rows left right comparers) table.rows }

let count_unchecked = count

let count table name =
  let* column = require_column table name in
  if not (is_categorical_sort column.sort) then
    Error "count requires a categorical column"
  else
    count_unchecked table name

let bin table name width =
  let* column = require_column table name in
  if not (is_numeric_sort column.sort) then Error "bin requires a numeric column"
  else if width <= 0 then Error "bin width must be positive"
  else
    let* numbers = get_column table name in
    let as_float = function
      | Int number -> Some (float_of_int number)
      | Float number -> Some number
      | Null -> None
      | _ -> None
    in
    let numbers = List.filter_map as_float numbers in
    match numbers with
    | [] -> of_rows [{ name = "group"; sort = String_sort }; { name = "count"; sort = Int_sort }] []
    | first :: remaining ->
        let minimum = List.fold_left min first remaining in
        let maximum = List.fold_left max first remaining in
        let width = float_of_int width in
        let first_bin = floor (minimum /. width) *. width in
        let rec make_bins lower rows =
          if lower > maximum then List.rev rows
          else
            let upper = lower +. width in
            let count = List.length (List.filter (fun number -> lower <= number && number < upper) numbers) in
            let label = Printf.sprintf "%g <= %s < %g" lower name upper in
            make_bins upper (["group", String label; "count", Int count] :: rows)
        in
        of_rows
          [{ name = "group"; sort = String_sort }; { name = "count"; sort = Int_sort }]
          (make_bins first_bin [])

let pivot_table table group_columns aggregates =
  let* () = require_columns table group_columns in
  let* () =
    if List.for_all (fun name ->
      match find_column table name with
      | Some column -> is_categorical_sort column.sort
      | None -> false) group_columns
    then Ok () else Error "pivotTable group columns must be categorical"
  in
  let aggregate_names = List.map (fun (name, _, _) -> name) aggregates in
  if not (unique (group_columns @ aggregate_names)) then Error "pivotTable output columns cannot collide"
  else
    let* () =
      let rec loop = function
        | [] -> Ok ()
        | (_, input, _) :: remaining ->
            let* _ = require_column table input in
            loop remaining
      in loop aggregates
    in
    let key row = List.map (fun name -> Option.get (find_in_row name row)) group_columns in
    let rec add_group groups row =
      let row_key = key row in
      match groups with
      | [] -> [row_key, [row]]
      | (existing_key, rows) :: remaining when existing_key = row_key ->
          (existing_key, row :: rows) :: remaining
      | group :: remaining -> group :: add_group remaining row
    in
    let groups = List.fold_left add_group [] table.rows in
    let aggregate_group (group_key, rows) =
      let rows = List.rev rows in
      let* aggregate_cells =
        let rec loop cells = function
          | [] -> Ok (List.rev cells)
          | (output, input, f) :: remaining ->
              let inputs = List.map (fun row -> Option.get (find_in_row input row)) rows in
              let* output_value = f inputs in
              loop ((output, output_value) :: cells) remaining
        in loop [] aggregates
      in
      Ok (List.combine group_columns group_key @ aggregate_cells)
    in
    let* rows =
      let rec loop kept = function
        | [] -> Ok (List.rev kept)
        | group :: remaining ->
            let* row = aggregate_group group in
            loop (row :: kept) remaining
      in loop [] groups
    in
    let group_schema = List.filter (fun column -> List.mem column.name group_columns) table.schema in
    let aggregate_schema =
      List.map2 (fun name (_, value) -> { name; sort = sort_of_value value }) aggregate_names
        (match rows with [] -> List.map (fun name -> name, Null) aggregate_names | row :: _ ->
          List.filter (fun (name, _) -> List.mem name aggregate_names) row)
    in
    of_rows (group_schema @ aggregate_schema) rows

let complete_cases table name =
  let* _ = require_column table name in
  Ok (List.map (fun row -> find_in_row name row <> Some Null) table.rows)

let dropna table =
  Ok { table with rows = List.filter (fun row -> not (List.exists (fun (_, value) -> value = Null) row)) table.rows }

let fillna table name replacement =
  let* column = require_column table name in
  if not (check_sort column.sort replacement) || replacement = Null then
    Error "fillna replacement must have the column sort"
  else
    let replace row = List.map (fun (cell_name, value) ->
      if cell_name = name && value = Null then cell_name, replacement else cell_name, value) row
    in
    Ok { table with rows = List.map replace table.rows }

let pivot_longer table names name_column value_column =
  if names = [] then Error "pivotLonger requires at least one column"
  else
    let* () = require_columns table names in
    let* first = require_column table (List.hd names) in
    if not (List.for_all (fun name ->
      match find_column table name with Some column -> column.sort = first.sort | None -> false) names) then
      Error "pivotLonger columns must have the same sort"
    else
      let retained_schema = List.filter (fun column -> not (List.mem column.name names)) table.schema in
      if not (unique (List.map (fun column -> column.name) retained_schema @ [name_column; value_column])) then
        Error "pivotLonger output columns cannot collide"
      else
        let rows = List.concat_map (fun row ->
          let retained = List.filter (fun (name, _) -> not (List.mem name names)) row in
          List.map (fun name ->
            retained @ [name_column, String name; value_column, Option.get (find_in_row name row)]) names
        ) table.rows in
        of_rows
          (retained_schema @ [{ name = name_column; sort = String_sort }; { name = value_column; sort = first.sort }])
          rows

let pivot_wider table name_column value_column =
  let* name_schema = require_column table name_column in
  let* value_schema = require_column table value_column in
  if name_schema.sort <> String_sort then Error "pivotWider name column must contain column names"
  else
    let retained_schema = List.filter (fun column -> column.name <> name_column && column.name <> value_column) table.schema in
    let* names = get_column table name_column in
    let generated_names = List.filter_map (function String name -> Some name | _ -> None) (remove_duplicates names) in
    if List.length generated_names <> List.length (remove_duplicates names)
       || not (unique (List.map (fun column -> column.name) retained_schema @ generated_names)) then
      Error "pivotWider generated column names are invalid"
    else
      let base_key row = List.filter (fun (name, _) -> name <> name_column && name <> value_column) row in
      let rec add_group groups row =
        let key = base_key row in
        match groups with
        | [] -> [key, [row]]
        | (existing_key, rows) :: remaining when existing_key = key -> (existing_key, row :: rows) :: remaining
        | group :: remaining -> group :: add_group remaining row
      in
      let groups = List.fold_left add_group [] table.rows in
      let make_row (key, rows) =
        let cells = List.map (fun generated_name ->
          let value = List.find_map (fun row ->
            match find_in_row name_column row with
            | Some (String name) when name = generated_name -> find_in_row value_column row
            | _ -> None) rows
          in generated_name, Option.value value ~default:Null) generated_names
        in key @ cells
      in
      of_rows
        (retained_schema @ List.map (fun name -> { name; sort = value_schema.sort }) generated_names)
        (List.map make_row groups)

let flatten table names =
  let* () = require_columns table names in
  if names = [] then Ok table
  else
    let* selected =
      let rec loop kept = function
        | [] -> Ok (List.rev kept)
        | name :: remaining ->
            let* column = require_column table name in
            match column.sort with
            | Sequence_sort element_sort -> loop ((name, element_sort) :: kept) remaining
            | _ -> Error "flatten requires sequence columns"
      in loop [] names
    in
    let expand row =
      let sequences = List.map (fun (name, _) ->
        match find_in_row name row with Some (Sequence values) -> values | Some Null -> [] | _ -> []) selected
      in
      match sequences with
      | [] -> Ok [row]
      | first :: remaining ->
          if not (List.for_all (fun values -> List.length values = List.length first) remaining) then
            Error "flattened sequences must have equal lengths within a row"
          else
            Ok (List.init (List.length first) (fun index ->
              List.map (fun (name, value) ->
                match List.assoc_opt name (List.combine (List.map fst selected) (List.map (fun values -> List.nth values index) sequences)) with
                | Some flattened -> name, flattened
                | None -> name, value) row))
    in
    let* rows =
      let rec loop kept = function
        | [] -> Ok (List.concat (List.rev kept))
        | row :: remaining ->
            let* expanded = expand row in
            loop (expanded :: kept) remaining
      in loop [] table.rows
    in
    let schema = List.map (fun column ->
      match List.assoc_opt column.name selected with
      | Some element_sort -> { column with sort = element_sort }
      | None -> column) table.schema
    in
    of_rows schema rows

let transform_column table name f =
  let* _ = require_column table name in
  let rows = List.map (fun row -> List.map (fun (cell_name, value) ->
    if cell_name = name then cell_name, f value else cell_name, value) row) table.rows
  in
  let output_values = List.filter_map (find_in_row name) rows in
  let schema = List.map (fun column ->
    if column.name = name then { column with sort = infer_sort output_values } else column) table.schema
  in
  of_rows schema rows

let rename_columns table renamings =
  let old_names = List.map fst renamings in
  let new_names = List.map snd renamings in
  let* () = require_columns table old_names in
  let remaining = List.filter (fun name -> not (List.mem name old_names)) (header table) in
  if not (unique (remaining @ new_names)) then Error "renameColumns would create duplicate column names"
  else
    let rename name = Option.value (List.assoc_opt name renamings) ~default:name in
    Ok {
      schema = List.map (fun column -> { column with name = rename column.name }) table.schema;
      rows = List.map (List.map (fun (name, value) -> rename name, value)) table.rows;
    }

let find table query =
  let* () =
    let rec loop = function
      | [] -> Ok ()
      | (name, value) :: remaining ->
          let* column = require_column table name in
          if check_sort column.sort value then loop remaining
          else Error ("query value has the wrong sort for column: " ^ name)
    in loop query
  in
  let rec search index = function
    | [] -> Error "not found"
    | row :: remaining ->
        if List.for_all (fun (name, value) -> find_in_row name row = Some value) query then Ok index
        else search (index + 1) remaining
  in search 0 table.rows

let update table f =
  let updates = List.map f table.rows in
  let* () =
    let rec validate = function
      | [] -> Ok ()
      | row :: remaining ->
          if not (unique (List.map fst row)) then Error "update rows cannot contain duplicate columns"
          else
            let* () = require_columns table (List.map fst row) in
            validate remaining
    in validate updates
  in
  let rows = List.map2 (fun row changes ->
    List.map (fun (name, value) -> name, Option.value (find_in_row name changes) ~default:value) row) table.rows updates
  in
  let schema =
    List.map (fun column ->
      let changed = List.filter_map (find_in_row column.name) updates in
      if changed = [] then column else { column with sort = infer_sort changed }) table.schema
  in of_rows schema rows

let select table f =
  let rows = List.mapi f table.rows in
  let* schema = schema_of_rows rows in
  of_rows schema rows

let select_many table project result =
  let rec collect index kept = function
    | [] -> Ok (List.rev kept)
    | row :: remaining ->
        let projected = project row index in
        let outputs = List.map (fun projected_row -> result row projected_row) projected.rows in
        collect (index + 1) (List.rev_append outputs kept) remaining
  in
  let* rows = collect 0 [] table.rows in
  let* schema = schema_of_rows rows in
  of_rows schema rows

let group_join first second get_key_first get_key_second aggregate =
  let rows = List.map (fun left ->
    let key = get_key_first left in
    let matches = List.filter (fun right -> get_key_second right = key) second.rows in
    aggregate left { second with rows = matches }) first.rows
  in
  let* schema = schema_of_rows rows in
  of_rows schema rows

let join first second get_key_first get_key_second combine =
  let rows = List.concat_map (fun left ->
    let key = get_key_first left in
    List.filter_map (fun right ->
      if get_key_second right = key then Some (combine left right) else None) second.rows
  ) first.rows in
  let* schema = schema_of_rows rows in
  of_rows schema rows

let group_by_retentive_unchecked = group_by_retentive

let group_by_retentive table name =
  let* column = require_column table name in
  if not (is_categorical_sort column.sort) then
    Error "groupByRetentive requires a categorical column"
  else group_by_retentive_unchecked table name

let group_by_subtractive_unchecked = group_by_subtractive

let group_by_subtractive table name =
  let* column = require_column table name in
  if not (is_categorical_sort column.sort) then
    Error "groupBySubtractive requires a categorical column"
  else group_by_subtractive_unchecked table name
