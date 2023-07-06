module CSP.Core.Expr

open CSP.Core.Val

type Expr<'P, 'E, 'V, 'C when 'V: comparison and 'C: comparison> =
    | LitNat of uint
    | LitBool of bool
    | LitTuple of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | Throw
    | IfExpr of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | MatchExpr of Expr<'P, 'E, 'V, 'C> * Map<Ctor<'C>, 'V * Expr<'P, 'E, 'V, 'C>> * ('V * Expr<'P, 'E, 'V, 'C>) option
    | VarRef of 'V
    | Ctor of Ctor<'C> * Expr<'P, 'E, 'V, 'C>
    | Not of Expr<'P, 'E, 'V, 'C>
    | And of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | Or of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | Eq of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | Less of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | TupleFst of Expr<'P, 'E, 'V, 'C>
    | TupleSnd of Expr<'P, 'E, 'V, 'C>
    | ListEmpty
    | ListCons of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | ListNth of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | ListLen of Expr<'P, 'E, 'V, 'C>
    | SetEmpty
    | SetRange of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | SetInsert of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | SetUnion of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | SetInter of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | SetDiff of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | SetMem of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | SetFilter of 'V * Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | SetExists of 'V * Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | MapEmpty
    | MapAdd of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>
    | MapFindOpt of Expr<'P, 'E, 'V, 'C> * Expr<'P, 'E, 'V, 'C>

let rec range (n1: uint32) (n2: uint32) : Set<Val<'C>> =
    if n1 > n2 then failwith "n1 > n2"
    else if n1 = n2 then Set.empty
    else Set.add (ValNat n1) (range (n1 + 1u) n2)
    
let addAll (m: Map<'K, 'V>) (ps: List<'K * 'V>): Map<'K, 'V> = List.fold (fun acc (k, v) -> Map.add k v acc) m ps

let rec eval (env: Map<'V, Val<'C>>) (expr: Expr<'P, 'E, 'V, 'C>) : Val<'C> =
    match expr with
    | LitNat n -> ValNat n
    | LitBool b -> ValBool b
    | LitTuple (l, r) -> ValTuple (eval env l, eval env r)
    | Throw -> Error
    | IfExpr (e1, e2, e3) ->
        match eval env e1 with
        | ValBool true -> eval env e2
        | ValBool false -> eval env e3
        | _ -> Error
    | MatchExpr (e, m, d) ->
        match eval env e with
        | ValUnion (c, v) ->
            match Map.tryFind c m with
            | Some (x, e1) ->
                if Map.containsKey x env
                then Error // NOTE: 変数シャドウはとりあえず落とす
                else eval (Map.add x v env) e1
            | None ->
                match d with
                | Some (x, e2) -> eval (Map.add x (ValUnion (c, v)) env) e2
                | None -> Error
        | _ -> Error
    | VarRef v -> Map.find v env
    | Ctor(c, e) -> ValUnion(c, eval env e)
    | Not e ->
        match eval env e with
        | ValBool b -> ValBool(not b)
        | _ -> Error
    | And(e1, e2) ->
        match eval env e1, eval env e2 with
        | ValBool b1, ValBool b2 -> ValBool(b1 && b2)
        | _ -> Error
    | Or(e1, e2) ->
        match eval env e1, eval env e2 with
        | ValBool b1, ValBool b2 -> ValBool(b1 || b2)
        | _ -> Error
    | Eq(e1, e2) ->
        match eval env e1, eval env e2 with
        | v1, v2 -> ValBool(v1 = v2)
    | Less(e1, e2) ->
        match eval env e1, eval env e2 with
        | ValBool b1, ValBool b2 -> ValBool(not b1 && b2)
        | ValNat n1, ValNat n2 -> ValBool(n1 < n2)
        | ValSet s1, ValSet s2 -> ValBool(Set.isProperSubset s1 s2)
        | _ -> Error
    | TupleFst e ->
        match eval env e with
        | ValTuple (l, _) -> l
        | _ -> Error
    | TupleSnd e ->
        match eval env e with
        | ValTuple (_, r) -> r
        | _ -> Error
    | ListEmpty -> ValList []
    | ListCons(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, ValList vs -> ValList(v :: vs)
        | _ -> Error
    | ListNth(e1, e2) ->
        match eval env e1, eval env e2 with
        | ValNat n, ValList vs -> List.item (Checked.int n) vs
        | _ -> Error
    | ListLen e ->
        match eval env e with
        | ValList vs -> ValNat(Checked.uint32 (List.length vs))
        | _ -> Error
    | SetEmpty -> ValSet Set.empty
    | SetRange(e1, e2) ->
        match eval env e1, eval env e2 with
        | ValNat n1, ValNat n2 when n1 <= n2 -> ValSet(range n1 n2)
        | _ -> Error
    | SetInsert(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, ValSet vs -> ValSet(Set.add v vs)
        | _ -> Error
    | SetUnion(e1, e2) ->
        match eval env e1, eval env e2 with
        | ValSet v1, ValSet v2 -> ValSet(Set.union v1 v2)
        | _ -> Error
    | SetInter(e1, e2) ->
        match eval env e1, eval env e2 with
        | ValSet v1, ValSet v2 -> ValSet(Set.intersect v1 v2)
        | _ -> Error
    | SetDiff(e1, e2) ->
        match eval env e1, eval env e2 with
        | ValSet v1, ValSet v2 -> ValSet(Set.difference v1 v2)
        | _ -> Error
    | SetMem(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, ValSet vs -> ValBool(Set.contains v vs)
        | _ -> Error
    | SetFilter(x, e1, e2) ->
        match eval env e2 with
        | ValSet vs ->
            (Set.fold
                (fun acc v ->
                    match acc with
                    | ValSet vsAcc ->
                        match eval (Map.add x v env) e1 with
                        | ValBool b -> ValSet(if b then Set.add v vsAcc else vs)
                        | _ -> Error
                    | _ -> Error)
                (ValSet Set.empty)
                vs)
        | _ -> Error
    | SetExists(x, e1, e2) ->
        match eval env e2 with
        | ValSet vs ->
            (Set.fold
                (fun acc v ->
                    match acc with
                    | ValBool true -> ValBool true
                    | ValBool false ->
                        match eval (Map.add x v env) e1 with
                        | ValBool b -> ValBool b
                        | _ -> Error
                    | _ -> Error)
                (ValBool false)
                vs)
        | _ -> Error
    | MapEmpty -> ValMap Map.empty
    | MapAdd(e1, e2, e3) ->
        match eval env e1, eval env e2, eval env e3 with
        | k, v, ValMap m -> ValMap(Map.add k v m)
        | _ -> Error
    | MapFindOpt(e1, e2) ->
        match eval env e1, eval env e2 with
        | k, ValMap m ->
            match Map.tryFind k m with
            | Some v -> ValUnion (BCtor BSome, v)
            | None -> ValUnion (BCtor BNone, ValUnit)
        | _ -> Error
