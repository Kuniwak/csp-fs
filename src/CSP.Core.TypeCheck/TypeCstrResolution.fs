module CSP.Core.TypeCstrResolution

open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.Util



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
        | TCUnit -> Ok TCUnit
        | TCTuple(tcL, tcR) ->
            (tcL, tcR)
            |> ResultEx.bind2 (resolve visited) (resolve visited)
            |> Result.map TCTuple
        | TCUnion(un, tcs) ->
            tcs
            |> ResultEx.bindAll (resolve visited)
            |> Result.map (fun tcs -> TCUnion(un, tcs))
        | TCSet tcV -> resolve visited tcV |> Result.map TCSet
        | TCList tcV -> resolve visited tcV |> Result.map TCList
        | TCMap(tcK, tcV) ->
            (tcK, tcV)
            |> ResultEx.bind2 (resolve visited) (resolve visited)
            |> Result.map TCMap

    resolve (Set []) t
