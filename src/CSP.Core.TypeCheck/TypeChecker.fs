module CSP.Core.TypeChecker

open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.GlobalEnv
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.TypeCstrEnv
open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.UnionMap
open CSP.Core.ProcMapTypeInference
open CSP.Core.Util

let typeEnv (um: UnionMap) (cm: CtorMap) (env: Env) (s: State) : Result<TypeCstrEnv * State, TypeError> =
    env
    |> Env.fold
        (fun accRes var v ->
            accRes
            |> Result.bind (fun (m, s) ->
                ValueTypeInference.infer um cm s v
                |> Result.map (fun (tc, s) -> (Map.add var tc m, s))))
        (Ok(Map.empty, s))
    |> Result.map (fun (m, s) -> (TypeCstrEnv m, s))

let typeGlobalEnv (um: UnionMap) (cm: CtorMap) (genv: GlobalEnv) (s: State) : Result<TypeCstrEnv * State, TypeError> =
    genv
    |> GlobalEnv.fold
        (fun accRes var expr ->
            accRes
            |> Result.bind (fun (m, s) ->
                ExprTypeInference.infer um cm empty expr s
                |> Result.map (fun (tc, s) -> (Map.add var (Expr.get tc) m, s))))
        (Ok(Map.empty, s))
    |> Result.map (fun (m, s) -> (TypeCstrEnv m, s))

let infer
    (um: UnionMap)
    (cm: CtorMap)
    (tcenv: TypeCstrEnv)
    (pm: ProcMap<unit>)
    (p: Proc<unit>)
    (s: State)
    : Result<(ProcMap<TypeCstr> * Proc<TypeCstr>) * State, TypeError> =
    infer um cm tcenv pm s
    |> Result.bind (fun (pm', s) ->
        ProcTypeInference.infer pm um cm tcenv p s
        |> Result.map (fun (p, s) -> ((pm', p), s)))

let postProcess (res: (ProcMap<TypeCstr> * Proc<TypeCstr>) * State) : Result<ProcMap<Type> * Proc<Type>, TypeError> =
    let pmp, s = res in

    pmp
    |> ResultEx.bind2 (resolve s) (ProcTypeInference.resolve s)
    |> Result.map (fun (pm, p) -> (instantiate pm, ProcTypeInference.instantiate p))