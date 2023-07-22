module CSP.Core.ClassOrd

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Ord"

let rec derivedBy (t: Type) : bool =
    match t with
    | TVar _ -> false
    | TNat -> true
    | TBool -> false
    | TTuple(ts) -> List.forall derivedBy ts
    | TSet _ -> true
    | TList _ -> false
    | TMap _ -> false
    | TUnion _ -> false

let rec less (v1: Val) (v2: Val) : bool =
    match v1, v2 with
    | VNat n1, VNat n2 -> n1 < n2
    | VTuple(vs1), VTuple(vs2) -> List.length vs1 = List.length vs2 && List.forall2 less vs1 vs2
    | VSet s1, VSet s2 -> Set.isSubset s1 s2
    | _, _ -> failwith $"cannot compare %s{format v1} vs %s{format v2}"
