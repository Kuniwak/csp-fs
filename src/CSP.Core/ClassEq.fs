module CSP.Core.ClassEq

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Eq"

let rec derivedBy (t: Type) : bool =
    match t with
    | TVar _ -> false
    | TNat _ -> true
    | TBool _ -> true
    | TTuple(ts, _) -> List.forall derivedBy ts
    | TSet(t, _) -> derivedBy t
    | TList(t, _) -> derivedBy t
    | TMap(tK, tV, _) -> derivedBy tK && derivedBy tV
    | TUnion(_, cm, _) -> Map.forall (fun _ -> List.forall derivedBy) cm

let rec eq (v1: Val) (v2: Val) : bool =
    match v1, v2 with
    | VNat n1, VNat n2 -> n1 = n2
    | VTuple(vs1), VTuple(vs2) -> List.length vs1 = List.length vs2 && List.forall2 eq vs1 vs2
    | VSet s1, VSet s2 -> s1 = s2 // TODO: use eq
    | VList vs1, VList vs2 -> List.length vs1 = List.length vs2 && List.forall2 eq vs1 vs2
    | VMap m1, VMap m2 -> m1 = m2 // TODO: use eq
    | VUnion(ctor1, vs1), VUnion(ctor2, vs2) ->
        ctor1 = ctor2 && List.length vs1 = List.length vs2 && List.forall2 eq vs1 vs2
    | _, _ -> failwith $"cannot compare %s{format v1} vs %s{format v2}"
