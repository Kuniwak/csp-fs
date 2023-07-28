module CSP.Core.TypeCstrResolution

open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState



let resolve (s: State) (t: TypeCstr) : Result<TypeCstr, TypeError> =
    let rec resolve visited t =
        match t with
        | TCUncertain u ->
            if Set.contains u visited then
                Error(Recursion(u, s.UncertainVarMap))
            else
                match resolveUncertainVar u s with
                | Some t -> resolve (Set.add u visited) t
                | None -> Ok(TCUncertain u)
        | TCBool -> Ok TCBool
        | TCNat -> Ok TCNat
        | TCTuple(ts) ->
            Result.map
                TCTuple
                (List.foldBack
                    (fun t tsOpt ->
                        match tsOpt, resolve visited t with
                        | Ok ts, Ok t -> Ok(t :: ts)
                        | Error err, _ -> Error err
                        | _, Error err -> Error err)
                    ts
                    (Ok []))
        | TCUnion(un, cm) ->
            let cmRes =
                Map.fold
                    (fun accRes ctor ts ->
                        let tsRes =
                            List.foldBack
                                (fun t -> Result.bind (fun ts -> Result.map (fun t -> t :: ts) (resolve visited t)))
                                ts
                                (Ok([]))

                        match accRes, tsRes with
                        | Ok cm, Ok ts -> Ok(Map.add ctor ts cm)
                        | Error err, _ -> Error err
                        | _, Error err -> Error err)
                    (Ok Map.empty)
                    cm in

            Result.map (fun cm -> TCUnion(un, cm)) cmRes
        | TCSet tcV -> Result.map TCSet (resolve visited tcV)
        | TCList tcV -> Result.map TCList (resolve visited tcV)
        | TCMap(tcK, tcV) ->
            Result.bind (fun tcK -> Result.map (fun tcV -> TCMap(tcK, tcV)) (resolve visited tcV)) (resolve visited tcK)

    resolve (Set []) t
