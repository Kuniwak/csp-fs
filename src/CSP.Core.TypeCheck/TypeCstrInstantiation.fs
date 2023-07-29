module CSP.Core.TypeCstrInstantiation

open CSP.Core.TypeCstr
open CSP.Core.Type

let rec instantiate (tc: TypeCstr) : Type =
    match tc with
    | TCUncertain(UncertainVarId u) -> TVar(u, __LINE__)
    | TCBool -> TBool(__LINE__)
    | TCNat -> TNat(__LINE__)
    | TCTuple(tcs) -> TTuple(List.map instantiate tcs, __LINE__)
    | TCUnion(un, cm) -> TUnion(un, Map.map (fun _ -> List.map instantiate) cm, __LINE__)
    | TCSet(tc) -> TSet(instantiate tc, __LINE__)
    | TCList(tc) -> TList(instantiate tc, __LINE__)
    | TCMap(tcK, tcV) -> TMap(instantiate tcK, instantiate tcV, __LINE__)

