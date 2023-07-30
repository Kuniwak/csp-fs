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
    env
    |> Env.fold
        (fun accRes var v ->
            accRes
            |> Result.bind (fun (m, s) ->
                ValueTypeInference.infer cm s v
                |> Result.map (fun (tc, s) -> (Map.add var tc m, s))))
        (Ok(Map.empty, s))
    |> Result.map (fun (m, s) -> (TypeCstrEnv m, s))


let typeCheck (cm: CtorMap) (genv: Env) (pm: ProcMap<unit>) : TypeError option =
    match typeEnv cm genv init with
    | Error(err) -> Some(err)
    | Ok(tcenv, s) ->
        let sRes =
            pm
            |> fold
                (fun sRes pn (vars, p) ->
                    sRes
                    |> Result.bind (fun s ->
                        let xs, s =
                            List.foldBack
                                (fun (var, t) (xs, s) -> let tc, s = generalize t s in ((var, tc) :: xs, s))
                                vars
                                ([], s)

                        let tcenv = bindAll xs tcenv in

                        match infer cm tcenv p s with
                        | Error(err) -> Error(At(err, $"process `{pn}`"))
                        | Ok(_, s) -> Ok(s)))
                (Ok(s))

        match sRes with
        | Ok _ -> None
        | Error terr -> Some terr
