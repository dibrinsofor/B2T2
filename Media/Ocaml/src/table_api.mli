type value = String of string | Int of int | Float of float | Bool of bool | Sequence of value list | Nested_table of t | Missing
and row = (string * value) list
and t
type 'a checked = ('a, string) result
val empty : t
val create : string list -> t checked
val of_rows : string list -> row list -> t checked
val header : t -> string list
val nrows : t -> int
val ncols : t -> int
val add_rows : t -> row list -> t checked
val row : t -> int -> row checked
val value : t -> int -> string -> value checked
val column : t -> string -> value list checked
val add_column : t -> string -> value list -> t checked
val build_column : t -> string -> (row -> value) -> t checked
val select_rows : t -> int list -> t checked
val select_rows_mask : t -> bool list -> t checked
val select_columns : t -> string list -> t checked
val filter : t -> (row -> bool) -> t
val drop_columns : t -> string list -> t checked
val rename_columns : t -> (string * string) list -> t checked
val vcat : t -> t -> t checked
val hcat : t -> t -> t checked
val cross_join : t -> t -> t checked
val complete_cases : t -> string -> bool list checked
val dropna : t -> t
val fillna : t -> string -> value -> t checked
val find : t -> row -> int checked
val string_of_value : value -> string
