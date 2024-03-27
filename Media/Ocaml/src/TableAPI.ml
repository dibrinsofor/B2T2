(* table *)
class type table_templ = object
  method columns: 'a list
  method add_rows: 'a list -> 'a list
end;;

class ['a] table  = object
  val mutable columns : 'a list = []
  method columns : 'a list = columns
  method add_rows : 'a list -> 'a list  = fun rows ->
    columns <- rows @ columns;
    columns
end;;

(* empty table *)
type empty = { }
let empty_rec = {}

class empty_table = object 
  val columns: empty_rec list = []
  end;;

let e = new empty_table;;

(* add rows *)
(* takes a table and a sequence of rows and returns a new table *)
(* each row's schema must match the table's schema *)

let add_rows t1: table rs: list = 
  match rs with
  | [] -> t1
  | h :: t -> t1#columns <- h :: columns
