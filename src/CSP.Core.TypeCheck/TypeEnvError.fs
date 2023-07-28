module CSP.Core.TypeEnvError

open CSP.Core.Var

type TypeEnvError =
    | UnboundVariable of Var
    | Shadow of Var * Set<Var>

let format (err: TypeEnvError) : string =
    match err with
    | UnboundVariable(var) -> $"unbound variable: %s{format var}"
    | Shadow(var, vars) ->
        let s = String.concat ", " (List.map format (Set.toList vars)) in $"shadow variable: %s{format var} in (%s{s})"
