module CSP.Core.Env

open CSP.Core.EnvError
open CSP.Core.Util
open CSP.Core.Val
open CSP.Core.Var

type Env = Env of Map<Var, Val>

let from (xs: (string * Val) seq) : Env =
    Env(Map [ for var, v in xs -> (Var var, v) ])

let empty: Env = Env Map.empty

let fold (f: 'State -> Var -> Val -> 'State) (s: 'State) (env: Env) =
    match env with
    | Env env -> Map.fold f s env

let bind1 (var: Var) (v: Val) (env: Env) : Env =
    match env with
    | Env env -> Env(Map.add var v env)

let bindAll (xs: (Var * Val) seq) (env: Env) : Env =
    Seq.fold (fun env (var, v) -> bind1 var v env) env xs

let bind1Opt (varOpt: Var option) (v: Val) (env: Env) : Env =
    match varOpt with
    | None -> env
    | Some(var) -> bind1 var v env

let bindAllOpts (xs: (Var option * Val) seq) (env: Env) : Env =
    Seq.fold (fun env (varOpt, v) -> bind1Opt varOpt v env) env xs

let declaredVars (env: Env) : Var list =
    match env with
    | Env env -> Seq.toList (Map.keys env)
    
let declared (var: Var) (env: Env): bool =
    match env with
    | Env env -> Map.containsKey var env

let valOf (var: Var) (env: Env) : Result<Val, EnvError> =
    match env with
    | Env env ->
        match Map.tryFind var env with
        | Some v -> Ok(v)
        | None -> Error(UnboundVariable(var, List.ofSeq (Map.keys env)))

let localVars (env: Env) (genv: Env) : (Var * Val) seq =
    match env, genv with
    | Env env, Env genv -> env |> Map.filter (fun var _ -> not (Map.containsKey var genv)) |> Map.toSeq

let format (genv: Env) (env: Env) : string =
    localVars env genv
    |> Seq.map (fun (var, v) -> $"{format var}={Val.format v}")
    |> String.concat ", "
    |> StringEx.wrapBy "{" "}"
