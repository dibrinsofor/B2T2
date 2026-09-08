open Table_api
let table header rows = match of_rows header rows with Ok t -> t | Error message -> failwith message
let students = table ["name"; "age"; "favorite color"] [
  ["name", String "Bob"; "age", Int 12; "favorite color", String "blue"];
  ["name", String "Alice"; "age", Int 17; "favorite color", String "green"];
  ["name", String "Eve"; "age", Int 13; "favorite color", String "red"]]
let students_missing = table ["name"; "age"; "favorite color"] [
  ["name", String "Bob"; "age", Missing; "favorite color", String "blue"];
  ["name", String "Alice"; "age", Int 17; "favorite color", String "green"];
  ["name", String "Eve"; "age", Int 13; "favorite color", Missing]]
let employees = table ["Last Name"; "Department ID"] [
  ["Last Name", String "Rafferty"; "Department ID", Int 31]; ["Last Name", String "Jones"; "Department ID", Int 32];
  ["Last Name", String "Heisenberg"; "Department ID", Int 33]; ["Last Name", String "Robinson"; "Department ID", Int 34];
  ["Last Name", String "Smith"; "Department ID", Int 34]; ["Last Name", String "Williams"; "Department ID", Missing]]
let departments = table ["Department ID"; "Department Name"] [
  ["Department ID", Int 31; "Department Name", String "Sales"]; ["Department ID", Int 33; "Department Name", String "Engineering"];
  ["Department ID", Int 34; "Department Name", String "Clerical"]; ["Department ID", Int 35; "Department Name", String "Marketing"]]
let gradebook = table ["name"; "age"; "quiz1"; "quiz2"; "midterm"; "quiz3"; "quiz4"; "final"] [
  ["name", String "Bob"; "age", Int 12; "quiz1", Int 8; "quiz2", Int 9; "midterm", Int 77; "quiz3", Int 7; "quiz4", Int 9; "final", Int 87];
  ["name", String "Alice"; "age", Int 17; "quiz1", Int 6; "quiz2", Int 8; "midterm", Int 88; "quiz3", Int 8; "quiz4", Int 7; "final", Int 85];
  ["name", String "Eve"; "age", Int 13; "quiz1", Int 7; "quiz2", Int 9; "midterm", Int 84; "quiz3", Int 8; "quiz4", Int 8; "final", Int 77]]
let gradebook_missing = table ["name"; "age"; "quiz1"; "quiz2"; "midterm"; "quiz3"; "quiz4"; "final"] [
  ["name", String "Bob"; "age", Int 12; "quiz1", Int 8; "quiz2", Int 9; "midterm", Int 77; "quiz3", Int 7; "quiz4", Int 9; "final", Int 87];
  ["name", String "Alice"; "age", Int 17; "quiz1", Int 6; "quiz2", Int 8; "midterm", Int 88; "quiz3", Missing; "quiz4", Int 7; "final", Int 85];
  ["name", String "Eve"; "age", Int 13; "quiz1", Missing; "quiz2", Int 9; "midterm", Int 84; "quiz3", Int 8; "quiz4", Int 8; "final", Int 77]]
let gradebook_seq = table ["name"; "age"; "quizzes"; "midterm"; "final"] [
  ["name", String "Bob"; "age", Int 12; "quizzes", Sequence [Int 8; Int 9; Int 7; Int 9]; "midterm", Int 77; "final", Int 87];
  ["name", String "Alice"; "age", Int 17; "quizzes", Sequence [Int 6; Int 8; Int 8; Int 7]; "midterm", Int 88; "final", Int 85];
  ["name", String "Eve"; "age", Int 13; "quizzes", Sequence [Int 7; Int 9; Int 8; Int 8]; "midterm", Int 84; "final", Int 77]]
let quiz_table grades = table ["quiz#"; "grade"] (List.mapi (fun i grade -> ["quiz#", Int (i + 1); "grade", Int grade]) grades)
let gradebook_table = table ["name"; "age"; "quizzes"; "midterm"; "final"] [
  ["name", String "Bob"; "age", Int 12; "quizzes", Nested_table (quiz_table [8; 9; 7; 9]); "midterm", Int 77; "final", Int 87];
  ["name", String "Alice"; "age", Int 17; "quizzes", Nested_table (quiz_table [6; 8; 8; 7]); "midterm", Int 88; "final", Int 85];
  ["name", String "Eve"; "age", Int 13; "quizzes", Nested_table (quiz_table [7; 9; 8; 8]); "midterm", Int 84; "final", Int 77]]
