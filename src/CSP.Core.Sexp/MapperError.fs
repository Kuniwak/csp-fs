module CSP.Core.Sexp.MapperError

open CSP.Core.Sexp.Sexp

type MapperError =
    | TooFewArguments of Sexp list
    | TooMuchArguments of Sexp list
    | UnexpectedAtom of string
    | UnexpectedList of Sexp list
    | UnexpectedEmpty

let format (err: MapperError) : string =
    match err with
    | TooFewArguments(ss) -> let s = ss |> Seq.map format |> String.concat " " in $"too few arguments: %s{s}"
    | TooMuchArguments(ss) -> let s = ss |> Seq.map format |> String.concat " " in $"too much arguments: %s{s}"
    | UnexpectedAtom(s) -> $"unexpected atom: %s{s}"
    | UnexpectedList(ss) -> let s = ss |> Seq.map format |> String.concat " " in $"unexpected list: (%s{s})"
    | UnexpectedEmpty -> "unexpected empty"
