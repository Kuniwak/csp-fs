module CSP.Core.Sexp.Parser

open CSP.Core.ProcMap
open CSP.Core.Type
open CSP.Core.Sexp.SyntaxError

let parse (str: string): Result<ProcMap<unit> * Type list, SyntaxError> =
    failwith ""