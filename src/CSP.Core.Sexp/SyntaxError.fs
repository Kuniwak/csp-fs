module CSP.Core.Sexp.SyntaxError

type SyntaxError =
    | ParensNotClosed
    | EmptyAtom
    | GarbageInTail of string

let format (err: SyntaxError) : string =
    match err with
    | ParensNotClosed -> "parenthesis is not closed"
    | EmptyAtom -> "must not be empty"
    | GarbageInTail(s) -> $"garbage in tail: %s{s}"
