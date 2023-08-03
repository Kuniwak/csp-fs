module CSP.Core.ValTypeCheck

open CSP.Core.Type
open CSP.Core.CtorMap
open CSP.Core.UnionMap
open CSP.Core.Val

let typeCheck (um: UnionMap) (cm: CtorMap) (t: Type) (v: Val) : bool =
    let rec typeCheck t v =
        match t, v with
        | TUnit _, VUnit -> true
        | TVar _, _ -> true
        | TNat _, VNat _ -> true
        | TBool _, VBool _ -> true
        | TTuple(tL, tR), VTuple(vL, vR) -> typeCheck tL vL && typeCheck tR vR
        | TSet(tElem), VSet s -> Set.forall (typeCheck tElem) s
        | TList(tElem), VList vs -> List.forall (typeCheck tElem) vs
        | TMap(tK, tV), VMap m -> Map.forall (fun k v -> typeCheck tK k && typeCheck tV v) m
        | TUnion(un, tm), VUnion(ctor, vs) ->
            match CtorMap.tryFind ctor cm with
            | Error _ -> false
            | Ok(un') ->
                if un = un' then
                    match instantiate ctor um tm cm with
                    | Ok(ts) -> List.length ts = List.length vs && List.forall2 typeCheck ts vs
                    | Error _ -> false
                else
                    false
        | _, _ -> false

    typeCheck t v
