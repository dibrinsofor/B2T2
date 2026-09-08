open Table_api

let passed = ref 0
let failed = ref 0

let check name expected actual show =
  if expected = actual then begin
    incr passed;
    Printf.printf "[PASS] %s\n  expected: %s\n  actual:   %s\n\n"
      name (show expected) (show actual)
  end else begin
    incr failed;
    Printf.printf "[FAIL] %s\n  expected: %s\n  actual:   %s\n\n"
      name (show expected) (show actual)
  end

let show_bool = string_of_bool
let show_int = string_of_int
let show_header names = "[" ^ String.concat "; " names ^ "]"

let sample_table : table = {
  schema = [
    { name = "name"; sort = String_sort };
    { name = "age"; sort = Int_sort };
  ];
  rows = [
    ["name", String "Bob"; "age", Int 12];
    ["name", String "Eve"; "age", Null];
  ];
}

let () =
  check "header derives names from schema"
    ["name"; "age"]
    (header sample_table)
    show_header;

  check "nrows counts rows"
    2
    (nrows sample_table)
    show_int;

  check "ncols counts schema columns"
    2
    (ncols sample_table)
    show_int;

  check "a string value matches String_sort"
    true
    (check_sort String_sort (String "Bob"))
    show_bool;

  check "an integer does not match String_sort"
    false
    (check_sort String_sort (Int 12))
    show_bool;

  check "Null is permitted in every sort"
    true
    (check_sort Int_sort Null)
    show_bool;

  Printf.printf "Summary: %d passed; %d failed\n" !passed !failed;
  if !failed > 0 then exit 1
