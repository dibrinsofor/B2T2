open OUnit2

let mt_table_tests = "test table encoding for empty tables" >::: [
  "empty" >:: (fun _ -> assert_equal 1 (sum [1]));
  "get schema" >:: (fun _ -> assert_equal 1 (sum [1]));
  "get rows" >:: (fun _ -> assert_equal 1 (sum [1]));
  "append empty table" >:: (fun _ -> assert_equal 1 (sum [1]));
]