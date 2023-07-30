module CSP.Core.ValueTypeInference

open CSP.Core.TypeInferenceState
open CSP.Core.TypeCstrUnification
open CSP.Core.TypeCstr
open CSP.Core.CtorMap
open CSP.Core.Val
open CSP.Core.TypeError

let infer (cm: CtorMap) (s: State) (v: Val) : Result<TypeCstr * State, TypeError> =
    let rec infer s v =
        match v with
        | VUnit -> Ok(TCUnit, s)
        | VNat _ -> Ok(TCNat, s)
        | VBool _ -> Ok(TCBool, s)
        | VTuple(vL, vR) ->
            infer s vL
            |> Result.bind (fun (tcL, s) -> infer s vR |> Result.map (fun (tcR, s) -> (TCTuple(tcL, tcR), s)))
        | VSet vs ->
            let u, s = newUncertainVarId s in

            let tRes =
                Set.fold
                    (fun tRes v ->
                        tRes
                        |> Result.bind (fun (t, s) -> infer s v |> Result.bind (fun (t', s) -> unify s t t')))
                    (Ok(TCUncertain u, s))
                    vs

            (Result.map (fun (t, s) -> (TCSet t, s)) tRes)
        | VList vs ->
            let u, s = newUncertainVarId s in

            let tRes =
                List.fold
                    (fun tRes v ->
                        Result.bind (fun (t, s) -> Result.bind (fun (t', s) -> unify s t t') (infer s v)) tRes)
                    (Ok(TCUncertain u, s))
                    vs

            Result.map (fun (t, s) -> (TCList t, s)) tRes
        | VMap m ->
            let uK, s = newUncertainVarId s in
            let uV, s = newUncertainVarId s in

            let tRes =
                Map.fold
                    (fun tRes vK vV ->
                        tRes
                        |> Result.bind (fun (tK, tV, s) ->
                            infer s vK
                            |> Result.bind (fun (tK', s) -> infer s vV |> Result.map (fun (tV', s) -> (tK', tV', s)))
                            |> Result.bind (fun (tK', tV', s) ->
                                unify s tK tK'
                                |> Result.bind (fun (tK, s) ->
                                    unify s tV tV' |> Result.map (fun (tV, s) -> (tK, tV, s))))))
                    (Ok(TCUncertain uK, TCUncertain uV, s))
                    m

            Result.map (fun (tK, tV, s) -> (TCMap(tK, tV), s)) tRes
        | VUnion(ctor, vs) ->
            match Map.tryFind ctor cm with
            | Some(un, cm) ->
                let cm, s = generalizeMap cm s in
                let tcs = Map.find ctor cm in

                if List.length tcs = List.length vs then
                    List.fold
                        (fun accRes (tc, v) ->
                            accRes
                            |> Result.bind (fun s ->
                                infer s v |> Result.bind (fun (tc', s) -> unify s tc tc') |> Result.map snd))
                        (Ok(s))
                        (List.zip tcs vs)
                    |> Result.map (fun s -> (TCUnion(un, cm), s))
                else
                    Error(
                        AssociatedValuesLenMismatch(
                            ctor,
                            Set[List.length tcs
                                List.length vs]
                        )
                    )
            | None -> Error(NoSuchCtor(ctor))

    infer s v
