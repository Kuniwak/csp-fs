module CSP.Core.Env

open CSP.Core.Val
open CSP.Core.Var

type Env = Map<Var, Val>

let format (env: Env) : string =
    let s =
        String.concat ", " (List.map (fun (var, v) -> $"{var}={format v}") (Map.toList env)) in

    $"{{{s}}}"
