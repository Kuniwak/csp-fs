module CSP.Core.TypeEnvError

open CSP.Core.Var

type TypeEnvError =
    | UnboundVariable of Var

let format (err: TypeEnvError) : string =
    match err with
    | UnboundVariable(var) -> $"unbound variable: %s{format var}"
