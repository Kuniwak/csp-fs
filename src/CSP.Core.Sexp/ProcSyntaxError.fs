module CSP.Core.Sexp.ProcSyntaxError

open CSP.Core.Sexp.ExprSyntaxError
open CSP.Core.Sexp.MapperError

type ProcSyntaxError =
    | At of ProcSyntaxError * string
    | MapperError of MapperError
    | ExprSyntaxError of ExprSyntaxError
    | UnexpectedKeyword of string

let rec format (err: ProcSyntaxError) : string =
    match err with
    | At(err, hint) -> $"%s{format err}\n\tat %s{hint}"
    | MapperError(err) -> MapperError.format err
    | ExprSyntaxError(err) -> ExprSyntaxError.format err
    | UnexpectedKeyword(kw) -> $"unexpected keyword: %s{kw}"
