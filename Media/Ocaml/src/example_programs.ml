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

let check_example_2 = 
  Random.self_init ();
  match sample_rows gradebook_missing 2 with 
  | Ok table -> print_table table
  | Error msg -> Printf.printf "%s" msg

(* pHackingHomogeneous *)
let p_hacking_homogeneous _table _threshold = ()
