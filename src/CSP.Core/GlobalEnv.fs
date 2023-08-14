module CSP.Core.GlobalEnv

open CSP.Core.EvalError
open CSP.Core.Util
open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.Eval
open CSP.Core.UnionMap
open CSP.Core.Var
open CSP.Core.Expr
open CSP.Core.GlobalEnvError

type GlobalEnv = GlobalEnv of Map<Var, Expr<unit>>

let toEnv (cfg: EvalConfig) (um: UnionMap) (cm: CtorMap) (genv: GlobalEnv) : Result<Env, EvalError> =
    let eval = eval cfg um cm empty in

    match genv with
    | GlobalEnv(genv) ->
        genv
        |> Map.toSeq
        |> ResultEx.bindAll (fun (var, expr) -> eval expr |> Result.map (fun v -> (var, v)))
        |> Result.map (fun xs -> bindAll xs empty)

let toSeq (genv: GlobalEnv) : (Var * Expr<unit>) seq =
    match genv with
    | GlobalEnv genv -> Map.toSeq genv

let fold (f: 'State -> Var -> Expr<unit> -> 'State) (s: 'State) (genv: GlobalEnv) =
    match genv with
    | GlobalEnv genv -> Map.fold f s genv


let declared (var: Var) (genv: GlobalEnv) : bool =
    match genv with
    | GlobalEnv(genv) -> Map.containsKey var genv

let bind (var: Var) (expr: Expr<unit>) (genv: GlobalEnv) : GlobalEnv =
    match genv with
    | GlobalEnv(genv) -> GlobalEnv(Map.add var expr genv)


let empty: GlobalEnv = GlobalEnv Map.empty

let from (xs: (string * Expr<unit>) list) : Result<GlobalEnv, GlobalEnvError> =
    xs
    |> List.fold
        (fun genvRes (var, expr) ->
            genvRes
            |> Result.bind (fun genv ->
                let var = Var var in

                if declared var genv then
                    Error(DuplicatedVar(var))
                else
                    Ok(bind var expr genv)))
        (Ok(empty))

let formatEntry (x: Var * Expr<unit>) : string =
    $"%s{Var.format (fst x)}: %s{Expr.format noAnnotation (snd x)}"
