module CSP.Core.ClassEq

open CSP.Core.Type
open CSP.Core.UnionMap
open CSP.Core.Val

let name: TypeClassName = "Eq"

let derivedBy (um: UnionMap) (t: Type) : bool =
    let rec derivedBy t =
        match t with
        | TUnit _ -> true
        | TNat _ -> true
        | TBool _ -> true
        | TTuple(tL, tR) -> derivedBy tL && derivedBy tR
        | TSet(t) -> derivedBy t
        | TList(t) -> derivedBy t
        | TMap(tK, tV) -> derivedBy tK && derivedBy tV
        | TUnion(un, tm) ->
            match instantiateCtorMap un tm um with
            | Error(err) -> failwith $"%s{UnionMapError.format err}"
            | Ok(cm) -> Map.forall (fun _ -> List.forall derivedBy) cm
        | _ -> false

    derivedBy t

let eq (v1: Val) (v2: Val) : Val =
    let rec eq v1 v2 =
        match v1, v2 with
        | VUnit, VUnit -> true
        | VNat n1, VNat n2 -> n1 = n2
        | VTuple(vL1, vR1), VTuple(vL2, vR2) -> eq vL1 vL2 && eq vR1 vR2
        | VSet s1, VSet s2 -> s1 = s2 // TODO: use eq
        | VList vs1, VList vs2 -> List.length vs1 = List.length vs2 && List.forall2 eq vs1 vs2
        | VMap m1, VMap m2 -> m1 = m2 // TODO: use eq
        | VUnion(ctor1, vs1), VUnion(ctor2, vs2) ->
            ctor1 = ctor2 && List.length vs1 = List.length vs2 && List.forall2 eq vs1 vs2
        | _, _ -> failwith $"cannot compare %s{format v1} vs %s{format v2}"

    VBool(eq v1 v2)
