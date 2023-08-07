module CSP.Core.TypeCstr

open CSP.Core.Type

type UncertainVarId = UncertainVarId of uint
type ForAllVarId = ForAllVarId of uint

type TypeCstr =
    | TCUncertain of UncertainVarId
    | TCUnit
    | TCNat
    | TCBool
    | TCTuple of TypeCstr * TypeCstr
    | TCSet of TypeCstr
    | TCList of TypeCstr
    | TCMap of TypeCstr * TypeCstr
    | TCUnion of UnionName * TypeCstr list

let rec format (tc: TypeCstr) : string =
    match tc with
    | TCUncertain(UncertainVarId id) -> $"?t%d{id}"
    | TCUnit -> "()"
    | TCNat -> "nat"
    | TCBool -> "bool"
    | TCTuple(tcL, tcR) -> $"(%s{format tcL}, %s{format tcR})"
    | TCSet tc -> $"(%s{format tc} set)"
    | TCList tc -> $"(%s{format tc} list)"
    | TCMap(tcK, tcV) -> $"((%s{format tcK}, %s{format tcV}) map)"
    | TCUnion(un, ts) -> let s = ts |> Seq.map format |> String.concat ", " in $"(%s{un} %s{s})"
