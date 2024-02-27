(* students: a simple table with no values missing *)
type student =
  {
    name: string option;
    age: int option;
    favourite_color: string option;
  }
let bob = {name = Some "Bob"; age = Some 12; favourite_color = Some "blue";}
let alice = {name = Some "Alice"; age = Some 17; favourite_color = Some "green";}
let eve = {name = Some "Eve"; age = Some 13; favourite_color = Some "red";}

class students = object 
  val mutable columns: student list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end
    
let s = new students;;
s#add_column bob;;
s#add_column alice;;
s#add_column eve;;
let table = s#get_columns;;
    

(* studentsMissing: a simple table with some values missing *)
let bob = {name = Some "Bob"; age = None; favourite_color = Some "blue";}
let alice = {name = Some "Alice"; age = Some 17; favourite_color = Some "green";}
let eve = {name = Some "Eve"; age = Some 13; favourite_color = None;}

class studentsMissing = object 
  val mutable columns: student list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let s = new studentsMissing;;
s#add_column bob;;
s#add_column alice;;
s#add_column eve;;
let table = s#get_columns;;


(* employees: a table that contains employees and their department IDs 
   ([source](https://en.wikipedia.org/wiki/Join_(SQL))) *)

type employee =
  {
    last_name: string option;
    department_id: int option;
  }

class employees = object
  val mutable columns: employee list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let e = new employees;;
e#add_column {last_name = Some "Rafferty"; department_id = Some 31;};;
e#add_column {last_name = Some "Jones"; department_id = Some 32;};;
e#add_column {last_name = Some "Heisenberg"; department_id = Some 33;};;
e#add_column {last_name = Some "Robinson"; department_id = Some 34;};;
e#add_column {last_name = Some "Smith"; department_id = Some 34;};;
e#add_column {last_name = Some "Williams"; department_id = None;};;
let table = e#get_columns;;

(* departments: a table that contains departments and their IDs 
   ([source](https://en.wikipedia.org/wiki/Join_(SQL))) *)

type department = 
{
  department_id: int option;
  departmen_name: string option;
}

class departments = object
  val mutable columns: department list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let d = new departments;;
d#add_column {department_id = Some 31; departmen_name = Some "Sales"};;
d#add_column {department_id = Some 33; departmen_name = Some "Engineering"};;
d#add_column {department_id = Some 34; departmen_name = Some "Clerical"};;
d#add_column {department_id = Some 35; departmen_name = Some "Marketing"};;
let table = d#get_columns;;


(* jellyAnon: a jelly bean table that contains only boolean data *)
type jellyComb = 
{
  get_acne: bool option;
  red: bool option;
  black: bool option;
  white: bool option;
  green: bool option;
  yellow: bool option;
  brown: bool option;
  orange: bool option;
  pink: bool option;
  purple: bool option;
}

class jellyAnon = object
  val mutable columns: jellyComb list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let j = new jellyAnon;;
j#add_column {get_acne = Some true; red = Some false; black = Some false; white = Some false; green = Some true; yellow = Some false; brown = Some false; orange = Some true; pink = Some false; purple = Some false};;
j#add_column {get_acne = Some true; red = Some false; black = Some true; white = Some false; green = Some true; yellow = Some true; brown = Some false; orange = Some false; pink = Some false; purple = Some false};;
j#add_column {get_acne = Some false; red = Some false; black = Some false; white = Some false; green = Some true; yellow = Some false; brown = Some false; orange = Some false; pink = Some true; purple = Some false};;
j#add_column {get_acne = Some false; red = Some false; black = Some false; white = Some false; green = Some false; yellow = Some true; brown = Some false; orange = Some false; pink = Some false; purple = Some false};;
j#add_column {get_acne = Some false; red = Some false; black = Some false; white = Some false; green = Some false; yellow = Some true; brown = Some false; orange = Some false; pink = Some true; purple = Some false};;
j#add_column {get_acne = Some true; red = Some false; black = Some true; white = Some false; green = Some false; yellow = Some false; brown = Some false; orange = Some true; pink = Some true; purple = Some false};;
j#add_column {get_acne = Some false; red = Some false; black = Some true; white = Some false; green = Some false; yellow = Some false; brown = Some false; orange = Some false; pink = Some true; purple = Some false};;
j#add_column {get_acne = Some true; red = Some false; black = Some false; white = Some false; green = Some false; yellow = Some false; brown = Some true; orange = Some true; pink = Some false; purple = Some false};;
j#add_column {get_acne = Some true; red = Some false; black = Some false; white = Some false; green = Some false; yellow = Some false; brown = Some false; orange = Some true; pink = Some false; purple = Some false};;
j#add_column {get_acne = Some false; red = Some true; black = Some false; white = Some false; green = Some false; yellow = Some true; brown = Some true; orange = Some false; pink = Some true; purple = Some false};;
let table = j#get_columns;;

(* jellyNamed: a jelly bean table that contains booleans and strings *)

type jellyWithName =
{
  name: string option;
  get_acne: bool option;
  red: bool option;
  black: bool option;
  white: bool option;
  green: bool option;
  yellow: bool option;
  brown: bool option;
  orange: bool option;
  pink: bool option;
  purple: bool option;
}

class jellyNamed = object
  val mutable columns: jellyWithName list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let j = new jellyNamed;;
j#add_column {name = Some "Emily"; get_acne = Some true; red = Some false; black = Some false; white = Some false; green = Some true; yellow = Some false; brown = Some false; orange = Some true; pink = Some false; purple = Some false};;
j#add_column {name = Some "Jacob"; get_acne = Some true; red = Some false; black = Some true; white = Some false; green = Some true; yellow = Some true; brown = Some false; orange = Some false; pink = Some false; purple = Some false};;
j#add_column {name = Some "Emma"; get_acne = Some false; red = Some false; black = Some false; white = Some false; green = Some true; yellow = Some false; brown = Some false; orange = Some false; pink = Some true; purple = Some false};;
j#add_column {name = Some "Aidan"; get_acne = Some false; red = Some false; black = Some false; white = Some false; green = Some false; yellow = Some true; brown = Some false; orange = Some false; pink = Some false; purple = Some false};;
j#add_column {name = Some "Madison"; get_acne = Some false; red = Some false; black = Some false; white = Some false; green = Some false; yellow = Some true; brown = Some false; orange = Some false; pink = Some true; purple = Some false};;
j#add_column {name = Some "Ethan"; get_acne = Some true; red = Some false; black = Some true; white = Some false; green = Some false; yellow = Some false; brown = Some false; orange = Some true; pink = Some true; purple = Some false};;
j#add_column {name = Some "Hannah"; get_acne = Some false; red = Some false; black = Some true; white = Some false; green = Some false; yellow = Some false; brown = Some false; orange = Some false; pink = Some true; purple = Some false};;
j#add_column {name = Some "Matthew"; get_acne = Some true; red = Some false; black = Some false; white = Some false; green = Some false; yellow = Some false; brown = Some true; orange = Some true; pink = Some false; purple = Some false};;
j#add_column {name = Some "Hailey"; get_acne = Some true; red = Some false; black = Some false; white = Some false; green = Some false; yellow = Some false; brown = Some false; orange = Some true; pink = Some false; purple = Some false};;
j#add_column {name = Some "Nicholas"; get_acne = Some false; red = Some true; black = Some false; white = Some false; green = Some false; yellow = Some true; brown = Some true; orange = Some false; pink = Some true; purple = Some false};;
let table = j#get_columns;;

(* ```lua
| name       | get acne | red   | black | white | green | yellow | brown | orange | pink  | purple |
| ---------- | -------- | ----- | ----- | ----- | ----- | ------ | ----- | ------ | ----- | ------ |
| "Jacob"    | true     | false | true  | false | true  | true   | false | false  | false | false  |
| "Emma"     | false    | false | false | false | true  | false  | false | false  | true  | false  |
| "Aidan"    | false    | false | false | false | false | true   | false | false  | false | false  |
| "Madison"  | false    | false | false | false | false | true   | false | false  | true  | false  |
| "Ethan"    | true     | false | true  | false | false | false  | false | true   | true  | false  |
| "Hannah"   | false    | false | true  | false | false | false  | false | false  | true  | false  |
| "Matthew"  | true     | false | false | false | false | false  | true  | true   | false | false  |
| "Hailey"   | true     | false | false | false | false | false  | false | true   | false | false  |
``` *)

(* gradebook: a gradebook table with no missing values *)

type grades = 
{
  name: string option;
  age: int option;
  quiz1: int option;
  quiz2: int option;
  midterm: int option;
  quiz3: int option;
  quiz4: int option;
  final: int option;
}

class gradebook = object
  val mutable columns: grades list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let g = new gradebook;;
g#add_column {name = Some "Bob"; age = Some 12; quiz1 = Some 8; quiz2 = Some 9; midterm = Some 77; quiz3 = Some 7; quiz4 = Some 9; final = Some 87};;
g#add_column {name = Some "Alice"; age = Some 17; quiz1 = Some 6; quiz2 = Some 8; midterm = Some 88; quiz3 = Some 8; quiz4 = Some 77; final = Some 85};;
g#add_column {name = Some "Eve"; age = Some 13; quiz1 = Some 7; quiz2 = Some 9; midterm = Some 84; quiz3 = Some 8; quiz4 = Some 8; final = Some 77};;
let table = g#get_columns;;

(* gradebookMissing: a gradebook table with some missing values *)

class gradebookMissing = object
  val mutable columns: grades list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let g = new gradebookMissing;;
g#add_column {name = Some "Bob"; age = Some 12; quiz1 = Some 8; quiz2 = Some 9; midterm = Some 77; quiz3 = Some 7; quiz4 = Some 9; final = Some 87};;
g#add_column {name = Some "Alice"; age = Some 17; quiz1 = Some 6; quiz2 = Some 8; midterm = Some 88; quiz3 = None; quiz4 = Some 77; final = Some 85};;
g#add_column {name = Some "Eve"; age = Some 13; quiz1 = None; quiz2 = Some 9; midterm = Some 84; quiz3 = Some 8; quiz4 = Some 8; final = Some 77};;
let table = g#get_columns;;

(* gradebookSeq: a gradebook table with sequence cells *)

type gradeSeq = 
{
  name: string option;
  age: int option;
  quizzes: int array option;
  midterm: int option;
  final: int option;
}

class gradebookSeq = object
  val mutable columns: gradeSeq list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let g = new gradebookSeq;;
g#add_column {name = Some "Bob"; age = Some 12; quizzes = Some [|8;9;7;9|]; midterm = Some 77; final = Some 87};;
g#add_column {name = Some "Alice"; age = Some 17; quizzes = Some [|6;8;8;7|]; midterm = Some 88; final = Some 85};;
g#add_column {name = Some "Eve"; age = Some 13; quizzes = Some [|7;9;8;8|]; midterm = Some 84; final = Some 77};;
let table = g#get_columns;;

(* gradebookTable: a gradebook table with table cells *)

type quiz =
  {
    quiz_number: int;
    grade: int option;
  }
type gradeTab = 
  {
    name: string option;
    age: int option;
    quizzes: quiz list option;
    midterm: int option;
    final: int option;
  }

class gradebookTable = object
  val mutable columns: gradeTab list = []
  method add_column c = 
    columns <- c :: columns
  method get_columns = 
    columns
  end

let g = new gradebookTable;;
g#add_column {name = Some "Bob"; age = Some 12; quizzes = Some [{quiz_number = 1; grade = Some 8;};{quiz_number = 2; grade = Some 9;};{quiz_number = 3; grade = Some 7;};{quiz_number = 4; grade = Some 9;}]; midterm = Some 77; final = Some 87};;
g#add_column {name = Some "Alice"; age = Some 17; quizzes = Some [{quiz_number = 1; grade = Some 6;};{quiz_number = 2; grade = Some 8;};{quiz_number = 3; grade = Some 8;};{quiz_number = 4; grade = Some 7;}]; midterm = Some 88; final = Some 85};;
g#add_column {name = Some "Eve"; age = Some 13; quizzes = Some [{quiz_number = 1; grade = Some 7;};{quiz_number = 2; grade = Some 9;};{quiz_number = 3; grade = Some 8;};{quiz_number = 4; grade = Some 8;}]; midterm = Some 84; final = Some 77};;
let table = g#get_columns;;