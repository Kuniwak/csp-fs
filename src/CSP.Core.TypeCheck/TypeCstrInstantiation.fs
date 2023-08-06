module CSP.Core.TypeCstrInstantiation

open CSP.Core.TypeCstr
open CSP.Core.TypeShorthand

let rec instantiate (tc: TypeCstr) : Type.Type =
    match tc with
    | TCUnit -> tUnit
    | TCUncertain(UncertainVarId u) -> tVar u
    | TCBool -> tBool
    | TCNat -> tNat
    | TCTuple(tcL, tcR) -> tTuple2 (instantiate tcL) (instantiate tcR)
    | TCUnion(un, tcs) -> tUnion un (tcs |> List.map instantiate)
    | TCSet(tc) -> tSet (instantiate tc)
    | TCList(tc) -> tList (instantiate tc)
    | TCMap(tcK, tcV) -> tMap (instantiate tcK) (instantiate tcV)
