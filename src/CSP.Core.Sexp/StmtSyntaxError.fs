module CSP.Core.Sexp.StmtSyntaxError

open CSP.Core.Sexp.ExprSyntaxError
open CSP.Core.Sexp.MapperError
open CSP.Core.Sexp.ProcSyntaxError
open CSP.Core.Sexp.TypeSyntaxError

type StmtSyntaxError =
    | At of StmtSyntaxError * string
    | ExprSyntaxError of ExprSyntaxError
    | TypeSyntaxError of TypeSyntaxError
    | ProcSyntaxError of ProcSyntaxError
    | MapperError of MapperError
    | UnexpectedKeyword of string
    
let rec format (err: StmtSyntaxError) : string =
    match err with
    | At(err, hint) -> $"%s{format err}\n\tat %s{hint}"
    | ExprSyntaxError(err) -> ExprSyntaxError.format err
    | TypeSyntaxError(err) -> TypeSyntaxError.format err
    | ProcSyntaxError(err) -> ProcSyntaxError.format err
    | MapperError(err) -> MapperError.format err
    | UnexpectedKeyword(kw) -> $"unexpected keyword: %s{kw}"
