module CSP.Core.UnivError

type UnivError =
    | UnivTVarIsNotAllowed
    | UnivTErrorIsNotAllowed

let format (err: UnivError) : string =
    match err with
    | UnivTVarIsNotAllowed -> "univ TVar is not allowed"
    | UnivTErrorIsNotAllowed -> "univ TError is not allowed"
