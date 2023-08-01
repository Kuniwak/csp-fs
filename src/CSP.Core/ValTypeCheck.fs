module CSP.Core.ValTypeCheck

open CSP.Core.Type
open CSP.Core.Val

let rec typeCheck (t: Type) (v: Val) : bool =
    match t, v with
    | TUnit _, VUnit -> true
    | TVar _, _ -> true
    | TNat _, VNat _ -> true
    | TBool _, VBool _ -> true
    | TTuple(tL, tR), VTuple(vL, vR) -> typeCheck tL vL && typeCheck tR vR
    | TSet(tElem), VSet s -> Set.forall (typeCheck tElem) s
    | TList(tElem), VList vs -> List.forall (typeCheck tElem) vs
    | TMap(tK, tV), VMap m -> Map.forall (fun k v -> typeCheck tK k && typeCheck tV v) m
    | TUnion(_, cm), VUnion(ctor, vs) ->
        match Map.tryFind ctor cm with
        | Some ts -> List.length ts = List.length vs && List.forall2 typeCheck ts vs
        | None -> false
    | _, _ -> false
