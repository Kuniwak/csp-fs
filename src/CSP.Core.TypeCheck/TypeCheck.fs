module CSP.Core.ProcMapTypeInference

open CSP.Core.TypeCstr
open CSP.Core.Env
open CSP.Core.TypeError
open CSP.Core.CtorMap
open CSP.Core.TypeCstrEnv
open CSP.Core.ProcMap
open CSP.Core.ProcTypeInference
open CSP.Core.TypeInferenceState

let typeEnv (cm: CtorMap) (env: Env) (s: State) : Result<TypeCstrEnv * State, TypeError> =
    let accRes =
        Env.fold
            (fun accRes var v ->
                Result.bind
                    (fun (m, s) -> Result.map (fun (tc, s) -> (Map.add var tc m, s)) (ValueTypeInference.infer cm s v))
                    accRes)
            (Ok(Map.empty, s))
            env in

    Result.map (fun (m, s) -> (TypeCstrEnv m, s)) accRes


let typeCheck (cm: CtorMap) (genv: Env) (pm: ProcMap<unit>) : TypeError option =
    match typeEnv cm genv init with
    | Error(err) -> Some(err)
    | Ok(tcenv, s) ->
        let sRes =
            fold
                (fun sRes pn (varOpts, p) ->
                    Result.bind
                        (fun s ->
                            let xs, s =
                                List.foldBack
                                    (fun varOpt (xs, s) ->
                                        let u, s = newUncertainVarId s in ((varOpt, TCUncertain u) :: xs, s))
                                    varOpts
                                    ([], s)

                            let tcenvRes =
                                Result.mapError (fun terr -> At(TypeEnvError terr, pn)) (bindAll xs tcenv)

                            Result.map snd (Result.bind (fun tcenv -> infer cm tcenv p s) tcenvRes))
                        sRes)
                (Ok(s))
                pm

        match sRes with
        | Ok _ -> None
        | Error terr -> Some terr
