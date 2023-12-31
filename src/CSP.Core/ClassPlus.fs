module CSP.Core.ClassPlus

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Plus"

let rec derivedBy (t: Type) : bool =
    match t with
    | TUnit _ -> true
    | TTuple(tL, tR) -> derivedBy tL && derivedBy tR
    | TBool _ -> true
    | TNat _ -> true
    | TSet _ -> true
    | TList _ -> true
    | _ -> false

let rec plus (v1: Val) (v2: Val) : Val =
    match v1, v2 with
    | VUnit, VUnit -> VUnit
    | VTuple(vL1, vR1), VTuple(vL2, vR2) -> VTuple(plus vL1 vL2, plus vR1 vR2)
    | VBool n1, VBool n2 -> VBool(n1 || n2)
    | VNat n1, VNat n2 -> VNat(n1 + n2)
    | VSet n1, VSet n2 -> VSet(Set.union n1 n2)
    | VList n1, VList n2 -> VList(List.append n1 n2)
    | _ -> failwith $"cannot plus: %s{format v1} vs %s{format v2}"
