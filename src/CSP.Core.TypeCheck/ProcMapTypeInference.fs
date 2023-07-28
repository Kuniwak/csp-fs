module CSP.Core.TypeInference.ProcMapTypeInference

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
                (fun sRes pn (varOpt, p) ->
                    Result.bind
                        (fun s ->
                            let u, s = newUncertainVarId s in

                            let tcenvRes =
                                match varOpt with
                                | None -> Ok(tcenv)
                                | Some(var) ->
                                    Result.mapError
                                        (fun terr -> At(TypeEnvError terr, pn))
                                        (bind1 var (TCUncertain u) tcenv)

                            Result.bind (fun tcenv -> infer cm tcenv p s) tcenvRes)
                        sRes)
                (Ok(s))
                pm

        match sRes with
        | Ok _ -> None
        | Error terr -> Some terr
