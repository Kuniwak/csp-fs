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
        | VNat _ -> Ok(TCNat, s)
        | VBool _ -> Ok(TCBool, s)
        | VTuple vs ->
            let tsRes =
                List.foldBack
                    (fun v -> Result.bind (fun (ts, s) -> Result.map (fun (t, s) -> (t :: ts, s)) (infer s v)))
                    vs
                    (Ok([], s))

            Result.map (fun (ts, s) -> (TCTuple ts, s)) tsRes
        | VSet vs ->
            let u, s = newUncertainVarId s in

            let tRes =
                Set.fold
                    (fun tRes v ->
                        Result.bind (fun (t, s) -> Result.bind (fun (t', s) -> unify s t t') (infer s v)) tRes)
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
                        Result.bind
                            (fun (tK, tV, s) ->
                                Result.bind
                                    (fun (tK', tV', s) ->
                                        Result.bind
                                            (fun (tK, s) -> Result.map (fun (tV, s) -> (tK, tV, s)) (unify s tV tV'))
                                            (unify s tK tK'))
                                    (Result.bind
                                        (fun (tK', s) -> Result.map (fun (tV', s) -> (tK', tV', s)) (infer s vV))
                                        (infer s vK)))
                            tRes)
                    (Ok(TCUncertain uK, TCUncertain uV, s))
                    m

            Result.map (fun (tK, tV, s) -> (TCMap(tK, tV), s)) tRes
        | VUnion(ctor, vs) ->
            match Map.tryFind ctor cm with
            | Some(un, cm) ->
                let cm, s = generalizeMap cm s in
                let tcs = Map.find ctor cm in

                if List.length tcs = List.length vs then
                    let accRes =
                        List.fold
                            (fun accRes (tc, v) ->
                                Result.bind
                                    (fun s ->
                                        Result.bind
                                            (fun (tc', s) -> Result.map snd (unify s tc tc'))
                                            (infer s v))
                                    accRes)
                            (Ok(s))
                            (List.zip tcs vs)

                    Result.map (fun s -> (TCUnion(un, cm), s)) accRes
                else
                    Error(AssociatedValuesLenMismatch(ctor, Set [ List.length tcs; List.length vs ]))
            | None -> Error(NoSuchCtor(ctor))

    infer s v
