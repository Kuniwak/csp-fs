module CSP.Core.Env

open CSP.Core.Val

type Env<'Var, 'Ctor when 'Var: comparison and 'Ctor: comparison> = Map<'Var, Val<'Ctor>>

let format (env: Env<'Var, 'Ctor>) : string =
    let s =
        String.concat ", " (List.map (fun (var, v) -> $"{var}={format v}") (Map.toList env)) in

    $"{{{s}}}"
