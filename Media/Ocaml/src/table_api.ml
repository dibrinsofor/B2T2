open Core;;


let combine_headers_values headers values =
  List.zip headers values
  |> List.filter_map ~f:(function
      | Some header, value -> Some (header, value)
      | _ -> None
    )
;;
(* table *)
(* type 'a header = (string * 'a) list *)
type header = string list

(* let header_type header = 
 List.map (fun (_, second) -> second) header

class ['a] table (header : 'b header)  = object
  val mutable header : 'b header = header
  val mutable columns : 'a list = []
  method columns : 'a list = columns
  
  (* add rows, can't enforce the header *)
  method add_rows (rows : 'a list) : 'a list =
    columns <- rows @ columns;
    columns
    
  end;; *)
  
  class ['a] table (header : header)  = object
    val mutable header : header = header
    val mutable columns : 'a list = []
    method columns : 'a list = columns
    
    (* add rows, can't enforce the header *)
    method add_rows (rows : 'a) : 'a list =
      let new_cols = 
        match List.zip header rows
        | 
      columns <- rows @ columns;
      columns
end


header -> ["Name"; "Age"; "Middle"; "Score"; "More"]

rows -> [(Some "Name"; Some 10; Some "Dibit"; Some 90.2; None),
         (Some "Name"; Some 10; Some "Dibit"; Some 90.2; None),
         (Some "Name"; Some 10; Some "Dibit"; Some 90.2; None)]

columns -> [{"Name": "Name"; "Age": 10; "Middle": "Dibit"; "Score": 90.2; "More": None},
            {"Name": "Name"; "Age": 10; "Middle": "Dibit"; "Score": 90.2; "More": None},
            {"Name": "Name"; "Age": 10; "Middle": "Dibit"; "Score": 90.2; "More": None}]

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


(* todo: write tests *)
let split (col_name: string) (sep: string): string list =
  let len_sep = String.length sep in
  let rec split_helper col_name acc =
    try
      let idx = String.index col_name sep.[0] in
      let prefix = String.sub col_name 0 idx in
      let rest = String.sub col_name (idx + len_sep) (String.length col_name - idx - len_sep) in
      split_helper rest (if prefix <> "" then acc @ [prefix] else acc)
    with Not_found ->
      if col_name <> "" then
        acc @ [col_name]
      else
        acc
  in
  split_helper col_name [];;





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



(* todo: call it a day and write tests for the ones you have and change the example encodings. *)
(* last session *)
(* table *)

(* let safe_zip (xs : 'a list) (ys : 'b list) : ('a * 'b) list option =
  let rec zip acc xs ys =
    match xs, ys with
    | [], [] -> Some (List.rev acc)
    | x :: xs, y :: ys -> zip ((x, y) :: acc) xs ys
    | _ -> None
  in
  zip [] xs ys

type header = string list

class ['a] table (header : header)  = object
  val mutable header : header = header
  val mutable columns : 'a list = []
  method columns : 'a list = columns

  (* add rows, can't enforce the header *)
  method add_rows (rows : 'a list) : 'a list =
    match safe_zip header rows with
    | Some zipped_rows ->
      columns <- zipped_rows :: columns;
      columns
    | None -> failwith "Mismatched lengths of header and rows"



end;;

let split (col_name: string) (sep: string): string list =
  let len_sep = String.length sep in
  let rec split_helper col_name acc =
    try
      let idx = String.index col_name sep.[0] in
      let prefix = String.sub col_name 0 idx in
      let rest = String.sub col_name (idx + len_sep) (String.length col_name - idx - len_sep) in
      split_helper rest (if prefix <> "" then acc @ [prefix] else acc)
    with Not_found ->
      if col_name <> "" then
        acc @ [col_name]
      else
        acc
  in
  split_helper col_name [];;

let even (num: int): bool =
 if num mod 2 ==  0 then 
   true
 else
   false;;

even 2;;
even 5;;

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

(* sequences, iters, lists, arrays? *)
(* or just claim to use only lists and ignore everything else  *)
(* let length (iters: 'a Seq.t): int =
  let rec length_helper acc seq =
    match Seq.next seq with
    | Some (_, rest) -> length_helper (acc + 1) rest
    | None -> acc
  in
  length_helper 0 iters;; *)


(* let extend_record record new_vs col_name = 
 { record with col_name = new_vs };;

let copy (existing_rows: 'a list) (new_rows: 'a list) (fn: 'a list -> 'b list): 'b list = 
  if List.length existing_rows != List.length new_rows then
    List.map2 extend_record existing_rows new_rows
    (* [<int; str>] -> [<int;str;int>]  *)
  else

let add_column (t1: 'b table) (col_name: string) (vs: 'a list): 'c table =
 new table [];;
 *)
(* let add_column : type a b c. b table -> string -> a list -> c table =
  fun t1 col_name vs -> 
   old_header = t1#header
   new_header = old_header @ col_name

   old_rows = t1#columns (* [<int; str>, <int; str>]*)
   let copy = fun old_rows fn

   new table new_header;; *)

let rec col_name_exists (header: header) (col_name: string) =
  match header with
  | [] -> false
  | col :: rst -> if col_name = col then true else col_name_exists rst col_name

let rec find_index str lst index =
  match lst with
  | [] -> -1
  | hd :: tl ->
    if hd = str then index
    else find_index str tl (index + 1)

let find_string_index str lst =
  find_index str lst 0

let is_number_col (row: 'a) (col_name: string): bool =
  try
    let attr_value = Obj.field (Obj.repr row) (int_of_string col_name) in
    Obj.is_int attr_value
  with 
  | Not_found | Failure _ -> false 

let get_column (t1: 'a table) (col_name: string): 'a list = ....

let dot_product (t1 : 'a table) (col1: string) (col2: string): int = ....
(* check that both cols exist in the table *)
 if (col_name_exists t1#header col1) && (col_name_exists t1#header col2) then
(* check that both are numbers or compatible with dotproducts? *)
   if (is_number_col t1#columns[0] col1) &&  (is_number_col t1#columns[0] col2) then ....
     
 else

extract columns and calculate dotproduct *)
