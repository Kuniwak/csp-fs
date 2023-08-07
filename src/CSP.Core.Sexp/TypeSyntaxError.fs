module CSP.Core.Sexp.TypeSyntaxError

open CSP.Core.Sexp.MapperError

type TypeSyntaxError =
    | MapperError of MapperError
    | UnknownType of string
    | InvalidTVarId of string

let format (err: TypeSyntaxError) : string =
    match err with
    | MapperError(err) -> format err
    | UnknownType(s) -> $"unknown type: %s{s}"
    | InvalidTVarId(s) -> $"type variables must be like /'[0-9]+/, but come: %s{s}"
