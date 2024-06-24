module CSP.Core.ValTypeCheck

open CSP.Core.Type
open CSP.Core.CtorMap
open CSP.Core.UnionMap
open CSP.Core.Val

let typeCheck (um: UnionMap) (cm: CtorMap) (t: Type) (v: Val) : bool =
    let rec typeCheck t v =
        match t, v with
        | TVar _, _ -> true
        | TUnit, VUnit -> true
        | TNat, VNat _ -> true
        | TBool, VBool _ -> true
        | TTuple(tL, tR), VTuple(vL, vR) -> typeCheck tL vL && typeCheck tR vR
        | TSet(tElem), VSet s -> Set.forall (typeCheck tElem) s
        | TList(tElem), VList vs -> List.forall (typeCheck tElem) vs
        | TMap(tK, tV), VMap m -> Map.forall (fun k v -> typeCheck tK k && typeCheck tV v) m
        | TUnion(un, unionTs), VUnion(ctor, vs) ->
            match toCtorTypes ctor um unionTs cm with
            | Ok(un', ctorTs) ->
                un = un'
                && List.length ctorTs = List.length vs
                && List.forall2 typeCheck ctorTs vs
            | Error _ -> false
        | _, _ -> false

    typeCheck t v
