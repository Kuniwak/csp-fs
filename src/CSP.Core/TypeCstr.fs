module CSP.Core.TypeCstr

open CSP.Core.Ctor
open CSP.Core.Type

type UncertainVarId = UncertainVarId of uint
type ForAllVarId = ForAllVarId of uint

type TypeCstr =
    | TCUncertain of UncertainVarId
    | TCNat
    | TCBool
    | TCTuple of TypeCstr list
    | TCSet of TypeCstr
    | TCList of TypeCstr
    | TCMap of TypeCstr * TypeCstr
    | TCUnion of UnionName * Map<Ctor, TypeCstr list>

let rec format (tc: TypeCstr) : string =
    match tc with
    | TCUncertain(UncertainVarId id) -> $"?t%d{id}"
    | TCNat -> "nat"
    | TCBool -> "bool"
    | TCTuple(tcs) -> let s = String.concat " * " (List.map format tcs) in $"(%s{s})"
    | TCSet tc -> $"(%s{format tc} set)"
    | TCList tc -> $"(%s{format tc} list)"
    | TCMap(tcK, tcV) -> $"((%s{format tcK}, %s{format tcV}) map)"
    | TCUnion(un, cm) ->
        let s =
            String.concat
                "/"
                (List.map
                    (fun (ctor, tcs) ->
                        let s = String.concat ", " (List.map format tcs) in $"%s{Ctor.format ctor} [%s{s}]")
                    (Map.toList cm))

        $"(%s{un} %s{s})"
