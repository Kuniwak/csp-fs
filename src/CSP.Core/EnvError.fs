module CSP.Core.EnvError

open CSP.Core.LineNum
open CSP.Core.Var

type EnvError =
    | At of EnvError * string
    | UnboundVariable of Var * Var list

let rec format (err: EnvError) : string =
    match err with
    | At(err, hint) -> $"%s{format err}\n\tat %s{hint}"
    | UnboundVariable(var, vars) ->
        let s = String.concat "/" (List.map Var.format vars) in
        $"unbound variable: %s{Var.format var} (available: %s{s})"

let atLine (line: LineNum) (err: EnvError): EnvError = At(err, $"line %s{line}")