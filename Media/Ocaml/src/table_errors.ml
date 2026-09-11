open Table_api
open Example_tables

let numeric_column table name =
  match find_column table name with
  | None -> Error ("unknown column: " ^ name)
  | Some { sort = Int_sort | Float_sort; _ } -> Ok ()
  | Some _ -> Error ("scatter plot column must contain numbers: " ^ name)

let scatter_plot table first_column second_column =
  let* () = numeric_column table first_column in
  numeric_column table second_column

let categorical_column table name =
  match find_column table name with
  | None -> Error ("unknown column: " ^ name)
  | Some { sort = String_sort | Bool_sort; _ } -> Ok ()
  | Some _ -> Error ("pie chart category column must be categorical: " ^ name)

let positive_number = function
  | Int n -> n > 0
  | Float n -> n > 0.
  | _ -> false

let pie_chart table category_column count_column =
  let* () = categorical_column table category_column in
  let* () = numeric_column table count_column in
  let* counts = get_column table count_column in
  if List.for_all positive_number counts then
    Ok ()
  else
    Error ("pie chart values must be positive: " ^ count_column)

let rec map_checked f = function
  | [] -> Ok []
  | value :: remaining ->
      let* mapped_value = f value in
      let* mapped_remaining = map_checked f remaining in
      Ok (mapped_value :: mapped_remaining)

let build_column_checked table name f =
  let* values = map_checked f table.rows in
  add_column table name values

let students_schema = [
  { name = "name"; sort = String_sort };
  { name = "age"; sort = Int_sort };
  { name = "favorite color"; sort = String_sort };
]

(* B2T2: missingSchema. Rows cannot be constructed without a schema here. *)
let missing_schema =
  let* students = create [] in
  add_rows students [
    ["name", String "Bob"; "age", Int 12; "favorite color", String "blue"];
    ["name", String "Alice"; "age", Int 17; "favorite color", String "green"];
    ["name", String "Eve"; "age", Int 13; "favorite color", String "red"];
  ]

(* B2T2: missingRow *)
let missing_row = 
  let* students = create students_schema in
  add_rows students [
    ["name", String "Bob"; "favorite color", String "blue"];
    ["name", String "Alice"; "age", Int 17; "favorite color", String "green"];
    [] (*missing row*)
  ]

(* B2T2: missingCell *)
let missing_cell =
  let* students = create students_schema in
  add_rows students [
    ["name", String "Bob"; "favorite color", String "blue"]; (*no age*)
    ["name", String "Alice"; "age", Int 17; "favorite color", String "green"];
    ["name", String "Eve"; "age", Int 13; "favorite color", String "red"];
  ]

(* B2T2: swappedColumns *)
let swapped_columns =
  let* students = create students_schema in
  add_rows students [
    ["name", Int 12; "age", String "Bob"; "favorite color", String "blue"];
    ["name", Int 17; "age", String "Alice"; "favorite color", String "green"];
    ["name", Int 13; "age", String "Eve"; "favorite color", String "red"];
  ]

(* B2T2: schemaTooShort *)
let schema_too_short =
  let* students = create [
    { name = "name"; sort = String_sort };
    { name = "age"; sort = Int_sort };
  ] in
  add_rows students [
    ["name", String "Bob"; "age", Int 12; "favorite color", String "blue"];
    ["name", String "Alice"; "age", Int 17; "favorite color", String "green"];
    ["name", String "Eve"; "age", Int 13; "favorite color", String "red"];
  ]

(* B2T2: schemaTooLong *)
let schema_too_long =
  let* students = create [
    { name = "name"; sort = String_sort };
    { name = "age"; sort = Int_sort };
    { name = "favorite number"; sort = Int_sort };
    { name = "favorite color"; sort = String_sort };
  ] in
  add_rows students [
    ["name", String "Bob"; "age", Int 12; "favorite number", String "blue"];
    ["name", String "Alice"; "age", Int 17; "favorite number", String "green"];
    ["name", String "Eve"; "age", Int 13; "favorite number", String "red"];
  ]

(* Program-level error examples. Each function is the buggy program from
   Errors.md, represented as a checked runtime failure where necessary. *)
let mid_final =
  scatter_plot gradebook "mid" "final"

let black_and_white =
  let eat_black_and_white row =
    get_value row "black and white"
  in
  let rec apply_to_rows values = function
    | [] -> Ok (List.rev values)
    | row :: remaining ->
        let* value = eat_black_and_white row in
        apply_to_rows (value :: values) remaining
  in
  let* values = apply_to_rows [] jelly_anon.rows in
  add_column jelly_anon "eat black and white" values

let pie_count =
  let* summary = count jelly_anon "get acne" in
  pie_chart summary "value" "count"

let brown_get_acne =
  let brown_and_get_acne row =
    let* brown = get_value row "brown" in
    let* acne = get_value row "get acne" in
    match brown, acne with
    | Bool ate_brown, Bool got_acne -> Ok (Bool (ate_brown && got_acne))
    | _ -> Error "brown and get acne must be boolean columns"
  in
  let* brownAndGetAcneTable = build_column_checked jelly_named "part2" brown_and_get_acne in
  count brownAndGetAcneTable "brown and get acne"

let get_only_row =
  let alice_rows =
    tfilter students (fun row -> get_value row "name" = Ok (String "Alice"))
  in
  let* alice = get_row alice_rows 1 in
  get_value alice "favorite color"

let favorite_color =
  let participants_like_green row =
    match get_value row "favorite color" with
    | Ok (String color) -> color = "green"
    | Ok _ -> false
    | Error _ -> false
  in
  Ok (tfilter students participants_like_green)

let brown_jellybeans =
  let count_participants table _color =
    let* _ =
      match table.rows with
      | first_row :: _ -> get_value first_row "color"
      | [] -> Ok Null
    in
    let keep row =
      match get_value row "color" with
      | Ok (Bool ate_color) -> ate_color
      | _ -> false
    in
    Ok (nrows (tfilter table keep))
  in
  count_participants jelly_anon "brown"

let employee_to_department =
  let last_name_to_dept_id dept_tab name =
    let match_name row =
      match get_value row "Last Name" with
      | Ok (String last_name) -> last_name = name
      | _ -> false
    in
    let matched_tab =
      tfilter dept_tab match_name
    in
    let* matched_row = get_row matched_tab 0 in
    get_value matched_row "Department ID"
  in
  let department_name_for_employee employee =
    let* last_name = get_value employee "Last Name" in
    match last_name with
    | String name -> last_name_to_dept_id departments name
    | _ -> Error "Last Name must contain strings"
  in
  let* _ = map_checked department_name_for_employee employees.rows in
  build_column employees "Department Name" (fun employee ->
    match department_name_for_employee employee with
    | Ok department_id -> department_id
    | Error _ -> Null)

let print_checked name = function
  | Ok table ->
      Printf.printf "\n[UNEXPECTED OK] %s\n" name;
      print_table table
  | Error message ->
      Printf.printf "\n[EXPECTED ERROR] %s\n%s\n" name message

let print_all_errors () =
  print_checked "missingSchema" missing_schema;
  print_checked "missingRow" missing_row;
  print_checked "missingCell" missing_cell;
  print_checked "swappedColumns" swapped_columns;
  print_checked "schemaTooShort" schema_too_short;
  print_checked "schemaTooLong" schema_too_long
