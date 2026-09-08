open Table_api

let col name sort : column = { name; sort }

let students : table = {
  schema = [col "name" String_sort; col "age" Int_sort; col "favorite color" String_sort];
  rows = [
    ["name", String "Bob"; "age", Int 12; "favorite color", String "blue"];
    ["name", String "Alice"; "age", Int 17; "favorite color", String "green"];
    ["name", String "Eve"; "age", Int 13; "favorite color", String "red"];
  ];
}

let students_missing : table = {
  schema = [col "name" String_sort; col "age" Int_sort; col "favorite color" String_sort];
  rows = [
    ["name", String "Bob"; "age", Null; "favorite color", String "blue"];
    ["name", String "Alice"; "age", Int 17; "favorite color", String "green"];
    ["name", String "Eve"; "age", Int 13; "favorite color", Null];
  ];
}

let employees : table = {
  schema = [col "Last Name" String_sort; col "Department ID" Int_sort];
  rows = [
    ["Last Name", String "Rafferty"; "Department ID", Int 31];
    ["Last Name", String "Jones"; "Department ID", Int 32];
    ["Last Name", String "Heisenberg"; "Department ID", Int 33];
    ["Last Name", String "Robinson"; "Department ID", Int 34];
    ["Last Name", String "Smith"; "Department ID", Int 34];
    ["Last Name", String "Williams"; "Department ID", Null];
  ];
}

let departments : table = {
  schema = [col "Department ID" Int_sort; col "Department Name" String_sort];
  rows = [
    ["Department ID", Int 31; "Department Name", String "Sales"];
    ["Department ID", Int 33; "Department Name", String "Engineering"];
    ["Department ID", Int 34; "Department Name", String "Clerical"];
    ["Department ID", Int 35; "Department Name", String "Marketing"];
  ];
}

let jelly_anon : table = {
  schema = List.map (fun name -> col name Bool_sort)
    ["get acne"; "red"; "black"; "white"; "green"; "yellow"; "brown"; "orange"; "pink"; "purple"];
  rows = [
    ["get acne", Bool true; "red", Bool false; "black", Bool false; "white", Bool false; "green", Bool true; "yellow", Bool false; "brown", Bool false; "orange", Bool true; "pink", Bool false; "purple", Bool false];
    ["get acne", Bool true; "red", Bool false; "black", Bool true; "white", Bool false; "green", Bool true; "yellow", Bool true; "brown", Bool false; "orange", Bool false; "pink", Bool false; "purple", Bool false];
    ["get acne", Bool false; "red", Bool false; "black", Bool false; "white", Bool false; "green", Bool true; "yellow", Bool false; "brown", Bool false; "orange", Bool false; "pink", Bool true; "purple", Bool false];
    ["get acne", Bool false; "red", Bool false; "black", Bool false; "white", Bool false; "green", Bool false; "yellow", Bool true; "brown", Bool false; "orange", Bool false; "pink", Bool false; "purple", Bool false];
    ["get acne", Bool false; "red", Bool false; "black", Bool false; "white", Bool false; "green", Bool false; "yellow", Bool true; "brown", Bool false; "orange", Bool false; "pink", Bool true; "purple", Bool false];
    ["get acne", Bool true; "red", Bool false; "black", Bool true; "white", Bool false; "green", Bool false; "yellow", Bool false; "brown", Bool false; "orange", Bool true; "pink", Bool true; "purple", Bool false];
    ["get acne", Bool false; "red", Bool false; "black", Bool true; "white", Bool false; "green", Bool false; "yellow", Bool false; "brown", Bool false; "orange", Bool false; "pink", Bool true; "purple", Bool false];
    ["get acne", Bool true; "red", Bool false; "black", Bool false; "white", Bool false; "green", Bool false; "yellow", Bool false; "brown", Bool true; "orange", Bool true; "pink", Bool false; "purple", Bool false];
    ["get acne", Bool true; "red", Bool false; "black", Bool false; "white", Bool false; "green", Bool false; "yellow", Bool false; "brown", Bool false; "orange", Bool true; "pink", Bool false; "purple", Bool false];
    ["get acne", Bool false; "red", Bool true; "black", Bool false; "white", Bool false; "green", Bool false; "yellow", Bool true; "brown", Bool true; "orange", Bool false; "pink", Bool true; "purple", Bool false];
  ];
}

let jelly_named : table = {
  schema = col "name" String_sort :: jelly_anon.schema;
  rows = List.map2 (fun name row -> ("name", String name) :: row)
    ["Emily"; "Jacob"; "Emma"; "Aidan"; "Madison"; "Ethan"; "Hannah"; "Matthew"; "Hailey"; "Nicholas"]
    jelly_anon.rows;
}

let gradebook : table = {
  schema = List.map (fun name -> col name (if name = "name" then String_sort else Int_sort))
    ["name"; "age"; "quiz1"; "quiz2"; "midterm"; "quiz3"; "quiz4"; "final"];
  rows = [
    ["name", String "Bob"; "age", Int 12; "quiz1", Int 8; "quiz2", Int 9; "midterm", Int 77; "quiz3", Int 7; "quiz4", Int 9; "final", Int 87];
    ["name", String "Alice"; "age", Int 17; "quiz1", Int 6; "quiz2", Int 8; "midterm", Int 88; "quiz3", Int 8; "quiz4", Int 7; "final", Int 85];
    ["name", String "Eve"; "age", Int 13; "quiz1", Int 7; "quiz2", Int 9; "midterm", Int 84; "quiz3", Int 8; "quiz4", Int 8; "final", Int 77];
  ];
}

let gradebook_missing : table = {
  schema = gradebook.schema;
  rows = [
    ["name", String "Bob"; "age", Int 12; "quiz1", Int 8; "quiz2", Int 9; "midterm", Int 77; "quiz3", Int 7; "quiz4", Int 9; "final", Int 87];
    ["name", String "Alice"; "age", Int 17; "quiz1", Int 6; "quiz2", Int 8; "midterm", Int 88; "quiz3", Null; "quiz4", Int 7; "final", Int 85];
    ["name", String "Eve"; "age", Int 13; "quiz1", Null; "quiz2", Int 9; "midterm", Int 84; "quiz3", Int 8; "quiz4", Int 8; "final", Int 77];
  ];
}

let gradebook_seq : table = {
  schema = [col "name" String_sort; col "age" Int_sort; col "quizzes" (Sequence_sort (List.to_seq [Int_sort])); col "midterm" Int_sort; col "final" Int_sort];
  rows = [
    ["name", String "Bob"; "age", Int 12; "quizzes", Sequence [Int 8; Int 9; Int 7; Int 9]; "midterm", Int 77; "final", Int 87];
    ["name", String "Alice"; "age", Int 17; "quizzes", Sequence [Int 6; Int 8; Int 8; Int 7]; "midterm", Int 88; "final", Int 85];
    ["name", String "Eve"; "age", Int 13; "quizzes", Sequence [Int 7; Int 9; Int 8; Int 8]; "midterm", Int 84; "final", Int 77];
  ];
}

let quiz_table grades : table = {
  schema = [col "quiz#" Int_sort; col "grade" Int_sort];
  rows = List.mapi (fun index grade -> ["quiz#", Int (index + 1); "grade", Int grade]) grades;
}

let gradebook_table : table = {
  schema = [col "name" String_sort; col "age" Int_sort; col "quizzes" Table_sort; col "midterm" Int_sort; col "final" Int_sort];
  rows = [
    ["name", String "Bob"; "age", Int 12; "quizzes", Nested_table (quiz_table [8; 9; 7; 9]); "midterm", Int 77; "final", Int 87];
    ["name", String "Alice"; "age", Int 17; "quizzes", Nested_table (quiz_table [6; 8; 8; 7]); "midterm", Int 88; "final", Int 85];
    ["name", String "Eve"; "age", Int 13; "quizzes", Nested_table (quiz_table [7; 9; 8; 8]); "midterm", Int 84; "final", Int 77];
  ];
}
