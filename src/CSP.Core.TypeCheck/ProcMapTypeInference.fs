module CSP.Core.ProcMapTypeInference

open CSP.Core.TypeCstr
open CSP.Core.TypeEnv
open CSP.Core.TypeError
open CSP.Core.CtorMap
open CSP.Core.TypeCstrEnv
open CSP.Core.ProcMap
open CSP.Core.ProcTypeInference
open CSP.Core.TypeInference

let typeCheck (cm: CtorMap) (tenv: TypeEnv) (pm: ProcMap<unit>) : TypeError option =
    let tcenvRes = from tenv in

    match tcenvRes with
    | Error terr -> Some(TypeEnvError terr)
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
