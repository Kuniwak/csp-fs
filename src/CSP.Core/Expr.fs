module CSP.Core.Expr

open CSP.Core.Val
open CSP.Core.Env

type Expr<'Var, 'Ctor when 'Var: comparison and 'Ctor: comparison> =
    | LitUnit
    | LitNat of uint
    | LitBool of bool
    | LitTuple of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | LitUnion of Ctor<'Ctor> * Expr<'Var, 'Ctor>
    | Throw
    | IfExpr of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | MatchExpr of Expr<'Var, 'Ctor> * Map<Ctor<'Ctor>, 'Var * Expr<'Var, 'Ctor>> * ('Var * Expr<'Var, 'Ctor>) option
    | VarRef of 'Var
    | Ctor of Ctor<'Ctor> * Expr<'Var, 'Ctor>
    | Not of Expr<'Var, 'Ctor>
    | And of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Or of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Eq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Plus of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Time of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Sub of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Less of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | TupleFst of Expr<'Var, 'Ctor>
    | TupleSnd of Expr<'Var, 'Ctor>
    | ListEmpty
    | ListCons of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | ListNth of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | ListLen of Expr<'Var, 'Ctor>
    | SetEmpty
    | SetRange of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetInsert of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetMem of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetFilter of 'Var * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetExists of 'Var * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | MapEmpty
    | MapAdd of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | MapFindOpt of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>

let rec ofVal (v: Val<'Ctor>): Expr<'Var, 'Ctor> =
    match v with
    | VUnit -> LitUnit
    | VNat n -> LitNat n
    | VBool b -> LitBool b
    | VTuple(l, r) -> LitTuple(ofVal l, ofVal r)
    | VSet s -> Set.fold (fun acc v -> SetInsert (ofVal v, acc)) SetEmpty s
    | VList vs -> List.fold (fun acc v -> ListCons (ofVal v, acc)) ListEmpty vs
    | VMap m -> Map.fold (fun acc k v -> MapAdd ((ofVal k), (ofVal v), acc)) MapEmpty m
    | VUnion (c, v) -> LitUnion (c, ofVal v)
    | VError -> Throw

let rec range (n1: uint32) (n2: uint32) : Set<Val<'C>> =
    if n1 > n2 then failwith "n1 > n2"
    else if n1 = n2 then Set.empty
    else Set.add (VNat n1) (range (n1 + 1u) n2)

let addAll (m: Map<'K, 'V>) (ps: List<'K * 'V>) : Map<'K, 'V> =
    List.fold (fun acc (k, v) -> Map.add k v acc) m ps

let rec eval (env: Env<'Var, 'Ctor>) (expr: Expr<'Var, 'Ctor>) : Val<'Ctor> =
    match expr with
    | LitUnit -> VUnit
    | LitNat n -> VNat n
    | LitBool b -> VBool b
    | LitTuple(l, r) -> VTuple(eval env l, eval env r)
    | LitUnion(c, e) -> VUnion(c, eval env e)
    | Throw -> VError
    | IfExpr(e1, e2, e3) ->
        match eval env e1 with
        | VBool true -> eval env e2
        | VBool false -> eval env e3
        | _ -> VError
    | MatchExpr(e, m, d) ->
        match eval env e with
        | VUnion(c, v) ->
            match Map.tryFind c m with
            | Some(x, e1) ->
                if Map.containsKey x env then
                    VError // NOTE: 変数シャドウはとりあえず落とす
                else
                    eval (Map.add x v env) e1
            | None ->
                match d with
                | Some(x, e2) -> eval (Map.add x (VUnion(c, v)) env) e2
                | None -> VError
        | _ -> VError
    | VarRef v -> Map.find v env
    | Ctor(c, e) -> VUnion(c, eval env e)
    | Not e ->
        match eval env e with
        | VBool b -> VBool(not b)
        | _ -> VError
    | And(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(b1 && b2)
        | _ -> VError
    | Or(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(b1 || b2)
        | _ -> VError
    | Eq(e1, e2) ->
        match eval env e1, eval env e2 with
        | v1, v2 -> VBool(v1 = v2)
    | Less(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(b1 < b2)
        | VNat n1, VNat n2 -> VBool(n1 < n2)
        | VSet s1, VSet s2 -> VBool(Set.isProperSubset s1 s2)
        | _ -> VError
    | Plus(e1, e2) ->
        match eval env e1, eval env e2 with
        | VNat n1, VNat n2 -> VNat(n1 + n2)
        | VSet v1, VSet v2 -> VSet(Set.union v1 v2)
        | _ -> VError
    | Time(e1, e2) ->
        match eval env e1, eval env e2 with
        | VNat n1, VNat n2 -> VNat(n1 * n2)
        | VSet v1, VSet v2 -> VSet(Set.intersect v1 v2)
        | _ -> VError
    | Sub(e1, e2) ->
        match eval env e1, eval env e2 with
        | VNat n1, VNat n2 -> VNat(if n1 < n2 then 0u else n1 - n2)
        | VSet v1, VSet v2 -> VSet(Set.difference v1 v2)
        | _ -> VError
    | TupleFst e ->
        match eval env e with
        | VTuple(l, _) -> l
        | _ -> VError
    | TupleSnd e ->
        match eval env e with
        | VTuple(_, r) -> r
        | _ -> VError
    | ListEmpty -> VList []
    | ListCons(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, VList vs -> VList(v :: vs)
        | _ -> VError
    | ListNth(e1, e2) ->
        match eval env e1, eval env e2 with
        | VNat n, VList vs -> List.item (Checked.int n) vs
        | _ -> VError
    | ListLen e ->
        match eval env e with
        | VList vs -> VNat(Checked.uint32 (List.length vs))
        | _ -> VError
    | SetEmpty -> VSet Set.empty
    | SetRange(e1, e2) ->
        match eval env e1, eval env e2 with
        | VNat n1, VNat n2 when n1 <= n2 -> VSet(range n1 n2)
        | _ -> VError
    | SetInsert(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, VSet vs -> VSet(Set.add v vs)
        | _ -> VError
    | SetMem(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, VSet vs -> VBool(Set.contains v vs)
        | _ -> VError
    | SetFilter(x, e1, e2) ->
        match eval env e2 with
        | VSet vs ->
            (Set.fold
                (fun acc v ->
                    match acc with
                    | VSet vsAcc ->
                        match eval (Map.add x v env) e1 with
                        | VBool b -> VSet(if b then Set.add v vsAcc else vs)
                        | _ -> VError
                    | _ -> VError)
                (VSet Set.empty)
                vs)
        | _ -> VError
    | SetExists(x, e1, e2) ->
        match eval env e2 with
        | VSet vs ->
            (Set.fold
                (fun acc v ->
                    match acc with
                    | VBool true -> VBool true
                    | VBool false ->
                        match eval (Map.add x v env) e1 with
                        | VBool b -> VBool b
                        | _ -> VError
                    | _ -> VError)
                (VBool false)
                vs)
        | _ -> VError
    | MapEmpty -> VMap Map.empty
    | MapAdd(e1, e2, e3) ->
        match eval env e1, eval env e2, eval env e3 with
        | k, v, VMap m -> VMap(Map.add k v m)
        | _ -> VError
    | MapFindOpt(e1, e2) ->
        match eval env e1, eval env e2 with
        | k, VMap m ->
            match Map.tryFind k m with
            | Some v -> VUnion(CtorSome, v)
            | None -> VUnion(CtorNone, VUnit)
        | _ -> VError

let rec format (expr: Expr<'V, 'C>) : string =
    match expr with
    | LitUnit -> "()"
    | LitNat n -> n.ToString()
    | LitBool b -> b.ToString()
    | LitTuple(l, r) -> $"({format l}, {format r})"
    | LitUnion(c, e) -> $"({c} {format e})"
    | Throw -> "throw"
    | IfExpr(e1, e2, e3) -> $"if {format e1} then {format e2} else {format e3}"
    | MatchExpr(e, cs, d) ->
        let sep = " | " in
        let cs' = List.map (fun (c, (v, e')) -> $"{c} {v} -> {format e'}") (Map.toList cs) in

        match d with
        | Some(v, e') -> $"(match {format e} with {String.concat sep cs'} | {v} -> {format e'})"
        | None -> $"(match {format e} with {String.concat sep cs'})"
    | VarRef v -> $"{v}"
    | Ctor(c, e) -> $"({c} {format e})"
    | Not e -> $"(not ({format e}))"
    | And(e1, e2) -> $"({format e1} && {format e2})"
    | Or(e1, e2) -> $"({format e1} || {format e2})"
    | Eq(e1, e2) -> $"({format e1} = {format e2})"
    | Less(e1, e2) -> $"({format e1} < {format e2})"
    | Plus(e1, e2) -> $"({format e1} + {format e2})"
    | Time(e1, e2) -> $"({format e1} * {format e2})"
    | Sub(e1, e2) -> $"({format e1} - {format e2})"
    | TupleFst e -> $"(fst {format e})"
    | TupleSnd e -> $"(snd {format e})"
    | ListEmpty -> "List.empty"
    | ListCons(e1, e2) -> $"({format e1} :: {format e2})"
    | ListNth(e1, e2) -> $"(List.nth {format e1} {format e2})"
    | ListLen e -> $"(List.length {format e})"
    | SetEmpty -> "Set.empty"
    | SetRange(e1, e2) -> $"(Set.range {format e1} {format e2})"
    | SetInsert(e1, e2) -> $"(Set.add {format e1} {format e2})"
    | SetMem(e1, e2) -> $"(Set.contains {format e1} {format e2})"
    | SetFilter(v, e1, e2) -> $"(Set.filter (fun {v} -> {format e1}) {format e2})"
    | SetExists(v, e1, e2) -> $"(Set.exists (fun {v} -> {format e1}) {format e2})"
    | MapEmpty -> "Map.empty"
    | MapAdd(k, v, m) -> $"(Map.add {format k} {format v} {format m})"
    | MapFindOpt(e1, e2) -> $"(Map.findOpt {format e1} {format e2})"
