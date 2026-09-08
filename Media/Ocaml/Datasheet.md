## Reference

This directory contains an OCaml implementation of the B2T2 table model. It
uses the OCaml standard library and Dune 3.10. The benchmark source is this
repository's root, at the revision used by this checkout. This datasheet was
updated on 2026-09-07.

## Example Tables

Tables support heterogeneous cells through `Table_api.value`, a closed variant
over strings, integers, floats, booleans, sequences, nested tables, and
`Missing`. Missing data is represented explicitly by `Missing`, not by an
absent row field. Tables are immutable: every transforming operation returns a
new table.

The ten benchmark tables are encoded in `src/example_tables.ml`. Their mapping
is direct: ordered header strings and an ordered list of named rows. Sequences
and nested tables are supported (`Sequence` and `Nested_table`). The generic
representation checks duplicate headers and rectangular, duplicate-free rows
at construction time. It does not statically infer a table-specific schema;
that is deliberately a run-time invariant because headers may be computed.

## TableAPI

The implemented operations are `emptyTable`, row construction/addition,
`header`, `nrows`, `ncols`, `getRow`, `getValue`, name-based `getColumn`,
`addColumn`, `buildColumn`, index- and mask-based `selectRows`, name-based
`selectColumns`, `tfilter`, `dropColumns`, `renameColumns`, `vcat`, `hcat`,
`crossJoin`, `completeCases`, `dropna`, `fillna`, and `find`. Failures are
explicit `('a, string) result` values.

The remaining B2T2 operations are not yet implemented: `values`, `leftJoin`,
numeric and general ordering, `distinct`, aggregation/grouping, pivots,
flattening, transformation, update/select-many, and group/join variants. No
claim is made that those operations are inexpressible in OCaml; they are simply
outside this initial repaired core.

## Example Programs

`src/example_programs.ml` implements `dotProduct`, `sampleRows`, and a simple
`quizScoreFilter`. The other benchmark programs remain unimplemented.

## Errors

`src/errors.ml` gives checked constructions for malformed-row and duplicate-
schema cases. The module reports concise validation strings. The complete B2T2
error suite, including all table-use errors and richer source diagnostics, is
not yet implemented.
