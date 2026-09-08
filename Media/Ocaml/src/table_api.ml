(** Immutable, schema-checked core representation for B2T2 tables. *)

type sort =
  | String_sort
  | Int_sort
  | Float_sort
  | Bool_sort
  | Sequence_sort of sort
  | Table_sort
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

type 'a checked = ('a, string) result

let header (table: table) =
  List.map (fun col -> col.name) table.schema

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

let ( let* ) = Result.bind
let unique xs =
  let rec loop seen = function [] -> true | x :: xs -> not (List.mem x seen) && loop (x :: seen) xs in
  loop [] xs
let create header = if unique header then Ok { header; rows = [] } else Error "a table header cannot contain duplicate column names"

let normalize_row header row =
  let names = List.map fst row in
  if not (unique names) then Error "a row cannot contain a column more than once"
  else if List.length row <> List.length header || not (List.for_all (fun c -> List.mem c names) header)
  then Error "a row must contain exactly the table's columns"
  else Ok (List.map (fun c -> (c, Option.get (find_in_row c row))) header)
let of_rows header rows =
  let* table = create header in
  let rec loop acc = function
    | [] -> Ok { table with rows = List.rev acc }
    | row :: rest -> let* row = normalize_row header row in loop (row :: acc) rest
  in loop [] rows
let add_rows t rows = let* other = of_rows t.header rows in Ok { t with rows = t.rows @ other.rows }
let row t index = if index < 0 then Error "row index must be non-negative" else match List.nth_opt t.rows index with Some r -> Ok r | None -> Error "row index is outside the table"
let column t name = if not (List.mem name t.header) then Error ("unknown column: " ^ name) else Ok (List.map (fun r -> Option.get (find_in_row name r)) t.rows)
let value t index column = let* r = row t index in match find_in_row column r with Some v -> Ok v | None -> Error ("unknown column: " ^ column)
let add_column t name values =
  if List.mem name t.header then Error ("duplicate column: " ^ name)
  else if List.length values <> nrows t then Error "a new column needs one value per row"
  else Ok { header = t.header @ [name]; rows = List.map2 (fun r v -> r @ [name, v]) t.rows values }
let build_column t name f = add_column t name (List.map f t.rows)
let select_rows t indices =
  let rec loop acc = function [] -> Ok { t with rows = List.rev acc } | i :: is -> let* r = row t i in loop (r :: acc) is in loop [] indices
let select_rows_mask t mask =
  if List.length mask <> nrows t then Error "a row mask needs one boolean per row"
  else
    let rec keep selected mask rows = match mask, rows with
      | [], [] -> List.rev selected
      | true :: mask, row :: rows -> keep (row :: selected) mask rows
      | false :: mask, _ :: rows -> keep selected mask rows
      | _ -> assert false
    in Ok { t with rows = keep [] mask t.rows }
let select_columns t columns =
  if not (unique columns) then Error "selected columns cannot contain duplicates"
  else if not (List.for_all (fun c -> List.mem c t.header) columns) then Error "cannot select an unknown column"
  else Ok { header = columns; rows = List.map (fun r -> List.map (fun c -> c, Option.get (find_in_row c r)) columns) t.rows }
let filter t predicate = { t with rows = List.filter predicate t.rows }
let drop_columns t columns = select_columns t (List.filter (fun c -> not (List.mem c columns)) t.header)
let rename_columns t renames =
  let rename c = match List.assoc_opt c renames with Some c -> c | None -> c in
  let header = List.map rename t.header in
  if not (List.for_all (fun (old, _) -> List.mem old t.header) renames) then Error "cannot rename an unknown column"
  else if not (unique header) then Error "renaming would produce duplicate column names"
  else Ok { header; rows = List.map (List.map (fun (c, v) -> rename c, v)) t.rows }
let vcat left right = if left.header <> right.header then Error "vertical concatenation requires identical headers" else Ok { left with rows = left.rows @ right.rows }
let hcat left right =
  if nrows left <> nrows right then Error "horizontal concatenation requires equally many rows"
  else if not (unique (left.header @ right.header)) then Error "horizontal concatenation requires disjoint headers"
  else Ok { header = left.header @ right.header; rows = List.map2 ( @ ) left.rows right.rows }
let cross_join left right =
  if not (unique (left.header @ right.header)) then Error "cross join requires disjoint headers"
  else Ok { header = left.header @ right.header; rows = List.concat_map (fun a -> List.map (fun b -> a @ b) right.rows) left.rows }
let complete_cases t name = let* values = column t name in Ok (List.map ((<>) Missing) values)
let dropna t = filter t (fun r -> List.for_all (fun (_, v) -> v <> Missing) r)
let fillna t name replacement =
  if not (List.mem name t.header) then Error ("unknown column: " ^ name)
  else Ok { t with rows = List.map (List.map (fun (c, v) -> if c = name && v = Missing then c, replacement else c, v)) t.rows }
let find t target =
  let rec loop i = function [] -> Error "row is not in the table" | r :: rs -> if r = target then Ok i else loop (i + 1) rs in loop 0 t.rows
let string_of_value = function
  | String x -> Printf.sprintf "%S" x | Int x -> string_of_int x | Float x -> string_of_float x | Bool x -> string_of_bool x
  | Sequence _ -> "<sequence>" | Nested_table _ -> "<table>" | Missing -> "<missing>"
