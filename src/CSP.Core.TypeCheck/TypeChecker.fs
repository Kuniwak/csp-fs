module CSP.Core.TypeChecker

open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.UnionMap
open CSP.Core.ProcMapTypeInference

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

let typeCheck (um: UnionMap) (cm: CtorMap) (genv: Env) (pm: ProcMap<unit>) (p: Proc<unit>) : Result<unit, TypeError> =
    typeEnv um cm genv init
    |> Result.bind (fun (tcenv, s) ->
        infer um cm tcenv pm s
        |> Result.bind (fun (_, s) -> ProcTypeInference.infer pm um cm tcenv p s))
    |> Result.map (fun _ -> ())
