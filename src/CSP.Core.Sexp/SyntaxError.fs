module CSP.Core.Sexp.SyntaxError

type SyntaxError = ParensNotClosed | EmptyAtom | EmptyList

let format (err: SyntaxError): string =
    match err with
    | ParensNotClosed -> "parenthesis is not closed"
    | EmptyAtom -> "must not be empty"