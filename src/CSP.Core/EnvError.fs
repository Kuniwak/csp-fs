module CSP.Core.EnvError

open CSP.Core.Var

type EnvError =
    | Shadow of Var * Var list
    | UnboundVariable of Var * Var list

let format (err: EnvError) : string =
    match err with
    | Shadow(var, vars) ->
        let s = String.concat "/" (List.map format vars)
        $"the variable shadow others: %s{format var} (others: %s{s})"
    | UnboundVariable(var, vars) ->
        let s = String.concat "/" (List.map format vars)
        $"unbound variable: %s{format var} (available: %s{s})"
