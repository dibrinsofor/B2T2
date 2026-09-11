## Reference

> Q. Where can we learn about the programming medium covered by this datasheet?
> (Feel free to link to multiple kinds of artifacts: repositories, papers,
> videos, etc. Please also include version information where applicable.)

https://ocaml.org/

version: `The OCaml toplevel, version 5.0.0`

> Q. What is the URL of the version of the benchmark being used?

2026-09-11

> Q. On what date was this version of the datasheet last updated?

2024-06-13

> Q. If you are not using the latest benchmark available on that date, please
> explain why not.

## Example Tables

> Q. Do tables express heterogeneous data, or must data be homogenized?

Tables express heterogeneous data. Each column has an independently declared
sort, while rows contain one value compatible with each column’s sort, and
dynamically added columns currently use `Unknown_sort`.

> Q. Do tables capture missing data and, if so, how?

Yes, we use the Null constructor to track ommitted data. See: `type value = ...`
in `src/table_api.ml`. Null is compatible with every other type.

> Q. Are mutable tables supported? Are there any limitations?

No, tables are immutable records. Operations on tables construct and return new
tables (and values).

> You may reference, instead of duplicating, the responses to the above
> questions in answering those below:

> Q. Which tables are inexpressible? Why?

No, all the example tables are expressible.

> Q. Which tables are only partially expressible? Why, and what’s missing?

N/A

> Q. Which tables’ expressibility is unknown? Why?

N/A

> Q. Which tables can be expressed more precisely than in the benchmark? How?

N/A

> Q. How direct is the mapping from the tables in the benchmark to
> representations in your system? How complex is the encoding?

The mapping is direct. A table stores an ordered schema of named columns and an
ordered list of rows. The shape of each cell is noted as `(string * Value)`
allowing necessary operations to validate and normalize tables.

## TableAPI

> Q. Are there consistent changes made to the way the operations are
> represented?

> Q. Which operations are entirely inexpressible? Why?

> Q. Which operations are only partially expressible? Why, and what’s missing?

> Q. Which operations’ expressibility is unknown? Why?

> Q. Which operations can be expressed more precisely than in the benchmark?
> How?

## Example Programs

> Q. Which examples are inexpressible? Why?

None. All eight benchmark example programs are expressible in the current
representation and have corresponding implementations in `example_programs.ml`.

> Q. Which examples’ expressibility is unknown? Why?

N/A

> Q. Which examples, or aspects thereof, can be expressed especially precisely?
> How?

The structure is able to describe sequences, and nested tables for the tables
that rely on that functionality. Namely, `groupByRetentive`, and
`groupBySubtractive`. However, the exact schema and shape of these are only
checked at runtime.

> Q. How direct is the mapping from the pseudocode in the benchmark to
> representations in your system? How complex is the encoding?

The mapping is pretty on par with the pseudocode presented, aiming to retain
naming and the sequence of function calls as closely as possible. However, we
track and propagate errors where necessary. The main difference is that
operations which can fail return `Result` values, so the OCaml code uses pattern
matching or `let*` to propagate errors.

## Errors

> There are (at least) two parts to errors: representing the source program that
> causes the error, and generating output that explains it. The term “error
> situation” refers to a representation of the cause of the error in the program
> source.
>
> For each error situation it may be that the language:
>
> - isn’t expressive enough to capture it
> - can at least partially express the situation
> - prevents the program from being constructed
>
> Expressiveness, in turn, can be for multiple artifacts:
>
> - the buggy versions of the programs
> - the correct variants of the programs
> - the type system’s representation of the constraints
> - the type system’s reporting of the violation

> Q. Which error situations are known to be inexpressible? Why?

None are wholly inexpressible as runtime error situations: `table_errors.mli`
contains a checked representation of every benchmark case. However, a table
literal with no schema cannot be constructed, because the OCaml table record
always requires a schema field. That case is represented indirectly by
attempting to add named cells to a table with an empty schema.

An earlier implementation explored representing tables as lists of first class
records which would have been sufficient to surface many of these bugs at
compile time, but was insufficient to describe generic API operations.

> Q. Which error situations are only partially expressible? Why, and what’s
> missing?

All of the error situations are only partially expressible. The current table
representation stores schemas and column names at runtime, so malformed rows,
unknown columns, and invalid row indices are reported as `Error` values rather
than as OCaml compile-time errors.

> Q. Which error situations’ expressibility is unknown? Why?

N/A

> Q. Which error situations can be expressed more precisely than in the
> benchmark? How?

See above. The current runtime representation is less precise than the
benchmark’s ideal static constraints because column membership and value sorts
are not encoded in OCaml types.

> Q. Which error situations are prevented from being constructed? How?

A handful of OCaml type errors are prevented. For example, a filter predicate
that returns a String rather than a bool will not typecheck.

> Q. For each error situation that is at least partially expressible, what is
> the quality of feedback to the programmer?

Runtime failures are returned as descriptive strings. Unknown-column errors
usually name the invalid column, and row-index errors explain that the index is
outside the table. Malformed-row feedback is less precise, it reports that a row
does not match the schema, but does not currently track any information about
the schema it violates.

> Q. For each error situation that is prevented from being constructed, what is
> the quality of feedback to the programmer?

See above.
