open Table_api
open Example_tables

let getval lst idx =
    match List.nth_opt lst idx with
    | Some (Int n) -> n
    | _ -> 0

let sample lst idx =
  if idx < 0 || idx > List.length lst then 
    None
  else
    let weighted = List.map (fun x -> (Random.bits (), x)) lst  in
    let sorted = List.sort (fun (w1, _) (w2, _) -> compare w1 w2) weighted in

    let rec sample_pt opts rem acc =
      if rem = 0 then
        List.rev acc
      else match opts with
      | [] -> List.rev acc
      | (_, x) :: xs -> sample_pt xs (rem - 1) (x :: acc)
    in
    Some (sample_pt sorted idx [])

let fisher_test lst1 lst2 =
  if List.length lst1 <> List.length lst2 then
    Error "Both sequences must be of the same length"
  else
    let rec count false_false false_true true_false true_true = function
      | ([], []) -> Ok (false_false, false_true, true_false, true_true)
      | (Bool left :: lefts, Bool right :: rights) ->
          let next_counts =
            match left, right with
            | false, false -> false_false + 1, false_true, true_false, true_true
            | false, true -> false_false, false_true + 1, true_false, true_true
            | true, false -> false_false, false_true, true_false + 1, true_true
            | true, true -> false_false, false_true, true_false, true_true + 1
          in
          let false_false, false_true, true_false, true_true = next_counts in
          count false_false false_true true_false true_true (lefts, rights)
      | _ -> Error "fisherTest requires two boolean columns"
    in
    let factorial n =
      let rec loop total = function
        | 0 -> total
        | value -> loop (total *. float_of_int value) (value - 1)
      in
      loop 1.0 n
    in
    match count 0 0 0 0 (lst1, lst2) with
    | Error _ as error -> error
    | Ok (a, b, c, d) ->
        let numerator =
          factorial (a + b) *. factorial (c + d) *.
          factorial (a + c) *. factorial (b + d)
        in
        let denominator =
          factorial a *. factorial b *. factorial c *. factorial d *.
          factorial (a + b + c + d)
        in
        Ok (numerator /. denominator)

let remove_duplicates values =
  let rec loop seen acc = function
    | [] ->
      List.rev acc
    | values :: tail ->
      if List.mem values acc then
        loop seen acc tail
      else
        loop (values :: seen) (values :: acc) tail
  in
  loop [] [] values

(* dotProduct *)
let dot_product table c1 c2 =
  let ns_boxed = get_column table c1 in
  let ms_boxed = get_column table c2 in
  match ms_boxed, ns_boxed with
  | Ok ms, Ok ns -> 
    let indices = List.init (nrows table) Fun.id in
    let sum =
      List.map (fun i -> getval ns i * getval ms i) indices
    in
    Ok (List.fold_left (+) 0 sum)
  | _ -> Error "gbam!"


(* dot_product gradebook "quiz1" "quiz2" *)

(* sampleRows *)

let sample_rows table n_1 = 
  let rows = nrows table in
  if n_1 < 0 || n_1 > rows then
    Error "sample size must be between 0 and the number of table rows"
  else
    let range = List.init rows Fun.id in
    match sample range n_1 with
    | Some indices -> select_rows table indices
    | None -> Error "table has no rows"

(* Random.self_init (); *)
(* match sample_rows gradebook_missing 2 with *) 
(* | Ok table -> print_table table *)
(* | Error msg -> Printf.printf "%s" msg *)


(* pHackingHomogeneous *)
let p_hacking table =
  let col_name = "get acne" in
  match get_column table col_name, drop_columns table [col_name] with
  | Ok col_acne, Ok jelly_anon_ ->
    let rec iter_heads = function
    | [] -> Ok ()
    | head :: tail -> 
      match get_column jelly_anon_ head with
      | Error msg -> Error msg
      | Ok col_jb ->
        match fisher_test col_acne col_jb with
        | Ok p ->
          if p < 0.05 then (
            Printf.printf "We found a link between %s jelly beans and acne (p < 0.05)."
            head);
          iter_heads tail
        | Error msg -> Error msg
    in
    iter_heads (header jelly_anon_)
  | Error msg, _ | _, Error msg -> Error msg

let p_hacking_homogeneous table =
  p_hacking table

(* p_hacking_homogeneous jelly_anon *)

(* pHackingHeterogeneous *)
let p_hacking_hetero table =
  let col_name = "name" in
  match drop_columns table [col_name] with
  | Error msg -> Error msg
  | Ok v -> p_hacking v;; 

(* p_hacking_hetero jelly_named *) 


(* quizScoreFilter *)
let quiz_score_filter table col_name =
  let headers = List.filter
    (fun name -> String.starts_with ~prefix:"quiz" name)
    (header table) in
  let scores row = List.filter_map (fun name ->
    match find_in_row name row with
    | Some (Int score) -> Some score
    | _ -> None
  ) headers in
  let avg nums =
    let sum = List.fold_left (+) 0 nums in
    float_of_int sum /. float_of_int (List.length nums) in
  let averages = (fun row -> Float (avg (scores row))) in
    build_column table col_name averages
  
(* match quiz_score_filter gradebook "average-quiz" with *)
(* | Ok t -> print_table t *)
(* | Error msg -> Printf.printf "%s" msg *)

(* quizScoreSelect *)
let quiz_score_select table =
  let quiz_col_names = List.init 4 (fun i -> "quiz" ^ string_of_int (i + 1)) in
  let quiz_table = select_columns_3 table quiz_col_names in
  let avg nums =
    let sum = List.fold_left (+) 0 nums in
    float_of_int sum /. float_of_int (List.length nums) in
  let scores row = List.filter_map (fun name ->
    match find_in_row name row with
    | Some (Int score) -> Some score
    | _ -> None
  ) quiz_col_names in
  match quiz_table with
  | Error msg -> Error msg
  | Ok t -> 
    let compute_scores = (fun row -> Float (avg (scores row))) in
    match build_column t "average" compute_scores with
      | Error msg -> Error msg
      | Ok quiz_and_average -> 
        match get_column quiz_and_average "average" with
        | Ok cols ->
          add_column table "average-quiz" cols
        | Error msg -> Error msg

(* quiz_score_select gradebook *)

(* groupByRetentive *)
let table_of_col col_name vals =
  let t1 = {
    empty_table with
    rows =  List.map (fun _ -> []) vals
  } in
  add_column t1 col_name vals

let group_by_retentive table col_name =
  match (get_column table col_name) with
  | Error msg -> Error msg
  | Ok col ->
    match table_of_col "key" (remove_duplicates col)  with
    | Error msg -> Error msg
    | Ok keys -> 
      let make_group kr =
        let k = Option.get (find_in_row "key" kr) in
        let pred r =
          find_in_row col_name r = Some k
        in
        Nested_table {table with rows = List.filter pred table.rows}
    in
    build_column keys "groups" make_group

(* match group_by_retentive students "favorite color" with *)
(* | Error _ -> Printf.printf "bummmer" *)
(* | Ok t -> print_table t *)


(* groupBySubtractive *)
let group_by_subtractive table col_name =
  match get_column table col_name with
  | Error msg -> Error msg
  | Ok col ->
      match table_of_col "key" (remove_duplicates col) with
      | Error msg -> Error msg
      | Ok keys ->
          let rec make_groups groups = function
            | [] -> Ok (List.rev groups)
            | kr :: remaining ->
                match find_in_row "key" kr with
                | None -> Error "a key row has no key"
                | Some k ->
                    let matching_rows =
                      List.filter
                        (fun r -> find_in_row col_name r = Some k)
                        table.rows
                    in
                    let retained_group = { table with rows = matching_rows } in
                    match drop_columns retained_group [col_name] with
                    | Error msg -> Error msg
                    | Ok group ->
                        make_groups (Nested_table group :: groups) remaining
          in
          match make_groups [] keys.rows with
          | Error msg -> Error msg
          | Ok groups -> add_column keys "groups" groups

      
(* match group_by_subtractive students "favorite color" with *)
(* | Error _ -> Printf.printf "bummer" *)
(* | Ok t -> print_table t *)
