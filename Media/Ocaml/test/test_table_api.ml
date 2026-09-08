open Table_api

let fail message = raise (Failure message)
let expect condition message = if not condition then fail message
let expect_ok expected = function
  | Ok actual -> expect (actual = expected) "unexpected successful result"
  | Error message -> fail message

let table = match of_rows ["name"; "age"] [["name", String "Bob"; "age", Int 12]; ["name", String "Eve"; "age", Missing]] with
  | Ok table -> table | Error message -> fail message

let () =
  expect (Result.is_error (create ["x"; "x"])) "duplicate headers must fail";
  expect (Result.is_error (of_rows ["x"; "y"] [["x", Int 1]])) "non-rectangular rows must fail";
  expect (header table = ["name"; "age"]) "schema order must be preserved";
  expect_ok (Int 12) (value table 0 "age");
  (match add_column table "active" [Bool true; Bool false] with
   | Ok table -> expect (ncols table = 3) "add_column must extend the schema"
   | Error message -> fail message);
  expect_ok [true; false] (complete_cases table "age");
  (match select_rows table [1] with
   | Ok table -> expect (nrows table = 1) "select_rows must return one row"
   | Error message -> fail message);
  print_endline "All table_api tests passed."
