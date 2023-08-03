module CSP.Core.UnivError

open CSP.Core.CtorMapError
open CSP.Core.UnionMapError

type UnivError =
    | UnivTVarIsNotAllowed
    | UnivTErrorIsNotAllowed
    | CtorMapError of CtorMapError
    | UnionMapError of UnionMapError

let format (err: UnivError) : string =
    match err with
    | UnivTVarIsNotAllowed -> "univ TVar is not allowed"
    | UnivTErrorIsNotAllowed -> "univ TError is not allowed"
    | CtorMapError(err) -> CtorMapError.format err
    | UnionMapError(err) -> format err
