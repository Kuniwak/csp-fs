module CSP.Core.Env

open CSP.Core.Val

type Env<'Var, 'Ctor when 'Var: comparison and 'Ctor: comparison> = Map<'Var, Val<'Ctor>>

let format (env: Env<'Var, 'Ctor>) : string =
    if Map.isEmpty env then
        ""
    else
        let s =
            String.concat ", " (List.map (fun (var, v) -> $"{var}={v}") (Map.toList env)) in

        $" env={{{s}}}"
