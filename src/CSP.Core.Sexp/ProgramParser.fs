module CSP.Core.Sexp.ProgramParser

open CSP.Core.ProcMap
open CSP.Core.UnionMap
open CSP.Core.Sexp.SyntaxError

let parse (str: string): Result<ProcMap<unit> * UnionMap, SyntaxError> =
    failwith ""