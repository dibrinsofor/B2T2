(* table *)
type 'a header = (string * 'a) list

let header_type header = 
 List.map (fun (_, second) -> second) header

class ['a] table (header : 'b header)  = object
  val mutable header : 'b header = header
  val mutable columns : 'a list = []
  method columns : 'a list = columns

  (* add rows, can't enforce the header *)
  method add_rows (rows : 'a list) : 'a list =
    columns <- rows @ columns;
    columns

end;;

(* empty table *)
let empty_table = new table [];;

(* TODO: example table in encoding *)

let rec col_name_exists (header: 'a header) (col_name: string) =
  match header with
  | [] -> false
  | (col, _) :: rst -> if col_name = col then true else col_name_exists rst col_name

(* required ops on col names - concat, num_to_name, split *)
let concat (col1: string) (col2: string): string =
   col1 ^ col2

let col_name_of_Number (num: int): string = 
   string_of_int num

let string_of_char c = String.make 1 c;;

let explode str =
  let rec explode_inner cur_index chars = 
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [new_char])
    else chars in
  explode_inner 0 [];;

let rec implode chars =
  match chars with
    [] -> ""
    | h::t ->  string_of_char h ^ (implode t);;


(* 
"dogtag_colour" "_" -> ["dogtag"; "colour"]
"clubhouse" "club" -> ["house"]
"club" "og" -> ["club"]
*)
let split (col_name: string) (sep: string): string list = 
   let rec split_inner (str: string) (sep: string) (acc: string list) : string list = 
      match explode col_name with 
      | [] -> acc |> List.rev
      | fst :: rst -> 
          if string_of_char fst = sep then 
             (implode rst :: acc) 
          else 
             split_inner (implode rst) sep acc 
   in split_inner col_name sep [];;


List.map print_string (split "_dogtag" "_")
              

(* quiz score filter *)
(* 
  1. add a new col,
  2. filter header
  3. startswith
  4. get value
  5. length and sum? 
*)


(* add rows *)
(* takes a table and a sequence of rows and returns a new table *)
(* each row's schema must match the table's schema *)
(* encoded in table *)


