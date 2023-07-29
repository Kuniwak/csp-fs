module CSP.Core.ClassOrd

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Ord"

let rec derivedBy (t: Type) : bool =
    match t with
    | TUnit _ -> true
    | TNat _ -> true
    | TTuple(tL, tR) -> derivedBy tL && derivedBy tR
    | TSet _ -> true
    | _ -> false

let rec less (v1: Val) (v2: Val) : bool =
    match v1, v2 with
    | VNat n1, VNat n2 -> n1 < n2
    | VTuple(tL1, tR1), VTuple(tL2, tR2) -> less tL1 tL2 && less tR1 tR2
    | VSet s1, VSet s2 -> Set.isSubset s1 s2
    | _, _ -> failwith $"cannot compare %s{format v1} vs %s{format v2}"
