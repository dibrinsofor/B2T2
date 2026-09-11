## Implement B2T2 in Ocaml

### Limitations

- Table schemas, column names, and cell sorts are checked at runtime through
  `('a, string) result`; OCaml cannot reject unknown column names such as
  `"mid"` at compile time when they are ordinary strings.
- A dynamically added column infers its sort from concrete values. A column
  containing only `Null` values necessarily remains `Unknown_sort`.
- `Table_sort` records only that a cell contains a nested table. It does not yet
  store and compare that nested table's exact schema recursively.
- Most API preconditions are reported as runtime errors rather than represented
  as static OCaml type constraints.

### Credits

- Dibri Nsofor
