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
    | TCUnit -> "unit"
    | TCNat -> "nat"
    | TCBool -> "bool"
    | TCTuple(tcL, tcR) -> $"(tuple %s{format tcL} %s{format tcR})"
    | TCSet tc -> $"(set %s{format tc})"
    | TCList tc -> $"(list %s{format tc})"
    | TCMap(tcK, tcV) -> $"(map %s{format tcK} %s{format tcV})"
    | TCUnion(un, ts) ->
        if List.length ts = 0 then
            un
        else
            let s = ts |> Seq.map format |> String.concat " " in $"(%s{un} %s{s})"
