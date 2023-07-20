module CSP.Core.Env

open CSP.Core.Val
open CSP.Core.Var

type Env = Map<Var, Val>

let from: (Var * Val) seq -> Env = Map
let empty: Env = Map.empty

let format (genv: Env) (env: Env) : string =
    let env = Map.filter (fun var _ -> not (Map.containsKey var genv)) env
    let s =
        String.concat ", " (List.map (fun (var, v) -> $"{format var}={Val.format v}") (Map.toList env)) in

    $"{{{s}}}"
