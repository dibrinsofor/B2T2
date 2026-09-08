open Table_api
let dot_product table left right =
  Result.bind (column table left) (fun left ->
    Result.bind (column table right) (fun right ->
      let rec sum total = function
        | [], [] -> Ok total | Int x :: xs, Int y :: ys -> sum (total + (x * y)) (xs, ys)
        | _ -> Error "dotProduct requires two integer columns"
      in sum 0 (left, right)))
let sample_rows table count = select_rows table (List.init (min count (nrows table)) Fun.id)
let quiz_score_filter table threshold = filter table (fun row -> match List.assoc_opt "final" row with Some (Int score) -> score >= threshold | _ -> false)
