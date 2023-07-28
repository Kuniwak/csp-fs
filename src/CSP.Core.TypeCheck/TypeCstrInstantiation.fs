module CSP.Core.TypeCstrInstantiation

open CSP.Core.TypeCstr
open CSP.Core.Type

let rec instantiate (tc: TypeCstr) : Type =
    match tc with
    | TCUncertain(UncertainVarId u) -> TVar u
    | TCBool -> TBool
    | TCNat -> TNat
    | TCTuple(tcs) -> TTuple(List.map instantiate tcs)
    | TCUnion(un, cm) -> TUnion(un, Map.map (fun _ -> List.map instantiate) cm)
    | TCSet(tc) -> TSet(instantiate tc)
    | TCList(tc) -> TList(instantiate tc)
    | TCMap(tcK, tcV) -> TMap(instantiate tcK, instantiate tcV)

