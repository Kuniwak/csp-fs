module CSP.Core.Sexp.SyntaxError

type SyntaxError = ParensNotClosed | EmptyAtom | GarbageInTail

let format (err: SyntaxError): string =
    match err with
    | ParensNotClosed -> "parenthesis is not closed"
    | EmptyAtom -> "must not be empty"
    | GarbageInTail -> "garbage in tail"
