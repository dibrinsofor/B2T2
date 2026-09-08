open Table_api
let missing_cell = of_rows ["name"; "age"] [["name", String "Bob"]]
let duplicate_schema = create ["name"; "name"]
let schema_too_short = of_rows ["name"] [["name", String "Bob"; "age", Int 12]]
