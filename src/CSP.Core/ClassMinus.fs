module CSP.Core.ClassMinus

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Plus"

let rec derivedBy (t: Type) : bool =
    match t with
    | TUnit _ -> true
    | TTuple(tL, tR) -> derivedBy tL && derivedBy tR
    | TNat _ -> true
    | TSet _ -> true
    | _ -> false

let rec minus (v1: Val) (v2: Val) : Val =
    match v1, v2 with
    | VUnit, VUnit -> VUnit
    | VTuple(vL1, vR1), VTuple(vL2, vR2) -> VTuple(minus vL1 vL2, minus vR1 vR2)
    | VNat n1, VNat n2 -> VNat(n1 - n2)
    | VSet n1, VSet n2 -> VSet(Set.difference n1 n2)
    | _ -> failwith $"cannot minus: %s{format v1} vs %s{format v2}"
