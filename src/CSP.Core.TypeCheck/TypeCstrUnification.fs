module CSP.Core.TypeCstrUnification

open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.TypeCstrResolution


let unify (s: State) (tc1: TypeCstr) (tc2: TypeCstr) : Result<TypeCstr * State, TypeError> =
    let rec unify s tc1 tc2 =
        match resolve s tc1, resolve s tc2 with
        | Error(terr), _ -> Error terr // NOTE: Occurence check failed.
        | _, Error(terr) -> Error terr // NOTE: Occurence check failed.
        | Ok(tc1), Ok(tc2) ->
            match tc1, tc2 with
            | TCUncertain n, _ -> Ok(tc2, bindUncertainVar n tc2 s)
            | _, TCUncertain n -> Ok(tc1, bindUncertainVar n tc1 s)
            | TCUnit, TCUnit -> Ok(TCUnit, s)
            | TCBool, TCBool -> Ok(TCBool, s)
            | TCNat, TCNat -> Ok(TCNat, s)
            | TCTuple(tcL1, tcR1), TCTuple(tcL2, tcR2) ->
                Result.bind
                    (fun (tcL, s) -> Result.map (fun (tcR, s) -> (TCTuple(tcL, tcR), s)) (unify s tcR1 tcR2))
                    (unify s tcL1 tcL2)
            | TCUnion(un1, cm1), TCUnion(un2, cm2) ->
                if un1 = un2 then
                    let ctors1 = Set.ofSeq (Map.keys cm1) in
                    let ctors2 = Set.ofSeq (Map.keys cm2) in
                    let ctors = Set.union ctors1 ctors2 in

                    let cmRes =
                        Set.fold
                            (fun accRes ctor ->
                                match accRes with
                                | Error terr -> Error terr
                                | Ok(cm, m) ->
                                    match Map.tryFind ctor cm1, Map.tryFind ctor cm2 with
                                    | Some tcs1, Some tcs2 ->
                                        Result.map
                                            (fun (tcs, m) -> (Map.add ctor tcs cm, m))
                                            (List.foldBack
                                                (fun (tc1, tc2) ->
                                                    Result.bind (fun (tcs, m) ->
                                                        match unify m tc1 tc2 with
                                                        | Ok(tc, s) -> Ok(tc :: tcs, s)
                                                        | Error terr ->
                                                            Error(At(terr, $"the type list of %s{Ctor.format ctor}"))))
                                                (List.zip tcs1 tcs2)
                                                (Ok([], m)))
                                    | _ -> Error(CtorsMismatch(ctors1, ctors2)))
                            (Ok(Map.empty, s))
                            ctors

                    match cmRes with
                    | Ok(cm, s) -> Ok(TCUnion(un1, cm), s)
                    | Error terr -> Error terr
                else
                    Error(UnionNameMismatch(un1, un2))
            | TCList tcV1, TCList tcV2 ->
                match unify s tcV1 tcV2 with
                | Error terr ->
                    Error(At(terr, $"the element type of the list: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"))
                | Ok(tcV, s) -> Ok(TCList(tcV), s)
            | TCSet tcV1, TCSet tcV2 ->
                match unify s tcV1 tcV2 with
                | Error terr ->
                    Error(At(terr, $"the element type of the set: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"))
                | Ok(tcV, s) -> Ok(TCSet(tcV), s)
            | TCMap(tcK1, tcV1), TCMap(tcK2, tcV2) ->
                // NOTE: Do not use Result.mapError. It make hard to read.
                match unify s tcK1 tcK2 with
                | Error terr ->
                    Error(At(terr, $"the key type of the set: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"))
                | Ok(tcK, s) ->
                    match unify s tcV1 tcV2 with
                    | Error terr ->
                        Error(At(terr, $"the value type of the set: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"))
                    | Ok(tcV, s) -> Ok(TCMap(tcK, tcV), s)
            | _, _ -> Error(TypeMismatch(Set [ tc1; tc2 ]))

    unify s tc1 tc2
