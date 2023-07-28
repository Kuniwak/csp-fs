module CSP.Core.Env

open CSP.Core.EnvError
open CSP.Core.Val
open CSP.Core.Var

type Env = Env of Map<Var, Val>

let from: (Var * Val) seq -> Env = Map >> Env
let empty: Env = Env Map.empty


let bind1 (var: Var) (v: Val) (env: Env) : Result<Env, EnvError> =
    match env with
    | Env env ->
        if Map.containsKey var env then
            Error(Shadow(var, List.ofSeq (Map.keys env)))
        else
            Ok(Env(Map.add var v env))

let bindAll (xs: (Var option * Val) seq) (env: Env) : Result<Env, EnvError> =
    Seq.fold
        (fun envRes (varOpt, v) ->
            match varOpt with
            | Some var -> Result.bind (bind1 var v) envRes
            | None -> envRes)
        (Ok(env))
        xs

let declared (env: Env) : Var list =
    match env with
    | Env env -> Seq.toList (Map.keys env)

let valOf (var: Var) (env: Env) : Result<Val, EnvError> =
    match env with
    | Env env ->
        match Map.tryFind var env with
        | Some v -> Ok(v)
        | None -> Error(UnboundVariable(var, List.ofSeq (Map.keys env)))

let format (genv: Env) (env: Env) : string =
    match genv, env with
    | Env genv, Env env ->
        let env = Map.filter (fun var _ -> not (Map.containsKey var genv)) env

        let s =
            String.concat ", " (List.map (fun (var, v) -> $"{format var}={Val.format v}") (Map.toList env)) in

        $"{{{s}}}"
