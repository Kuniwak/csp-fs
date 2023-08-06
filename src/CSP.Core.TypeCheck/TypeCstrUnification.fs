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
            | TCUnion(un1, tcs1), TCUnion(un2, tcs2) ->
                if un1 = un2 then
                    if List.length tcs1 = List.length tcs2 then
                        List.foldBack
                            (fun (tc1, tc2) tcsRes ->
                                tcsRes
                                |> Result.bind (fun (tcs, s) ->
                                    unify s tc1 tc2 |> Result.map (fun (tc, s) -> (tc :: tcs, s))))
                            (List.zip tcs1 tcs2)
                            (Ok([], s))
                        |> Result.map (fun (tcs, s) -> (TCUnion(un1, tcs), s))
                    else
                        Error(
                            TypeArgumentsLengthMismatch(
                                Set[tcs1
                                    tcs2]
                            )
                        )
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
