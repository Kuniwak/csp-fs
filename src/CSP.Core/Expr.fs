module CSP.Core.Expr

open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Type
open CSP.Core.Val
open CSP.Core.Env

type Expr<'Var, 'Ctor when 'Var: comparison and 'Ctor: comparison> =
    | Lit of Val<'Ctor>
    | Union of Ctor<'Ctor> * Expr<'Var, 'Ctor>
    | UnionEq of string * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Throw
    | ErrorEq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | If of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Match of Expr<'Var, 'Ctor> * Map<Ctor<'Ctor>, 'Var * Expr<'Var, 'Ctor>> * ('Var option * Expr<'Var, 'Ctor>) option
    | VarRef of 'Var
    | BoolEq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | BoolNot of Expr<'Var, 'Ctor>
    | BoolAnd of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | BoolOr of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | NatEq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | NatLess of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | NatAdd of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | NatProd of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | NatSub of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Tuple of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | TupleEq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | TupleFst of Expr<'Var, 'Ctor>
    | TupleSnd of Expr<'Var, 'Ctor>
    | ListEmpty
    | ListEq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | ListCons of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | ListNth of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | ListLen of Expr<'Var, 'Ctor>
    | SetEmpty
    | SetEq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetRange of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetInsert of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetMem of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetFilter of 'Var * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetExists of 'Var * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | MapEmpty
    | MapEq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | MapAdd of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | MapFindOpt of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Univ of Type

let rec ofVal (v: Val<'Ctor>) : Expr<'Var, 'Ctor> =
    match v with
    | VUnit -> Lit v
    | VNat _ -> Lit v
    | VBool _ -> Lit v
    | VTuple(l, r) -> Tuple(ofVal l, ofVal r)
    | VSet s -> Set.fold (fun acc v -> SetInsert(ofVal v, acc)) SetEmpty s
    | VList vs -> List.fold (fun acc v -> ListCons(ofVal v, acc)) ListEmpty vs
    | VMap m -> Map.fold (fun acc k v -> MapAdd((ofVal k), (ofVal v), acc)) MapEmpty m
    | VUnion(c, v) -> Union(c, ofVal v)
    | VError -> Throw

let rec range (n1: uint32) (n2: uint32) : Set<Val<'C>> =
    if n1 > n2 then failwith "n1 > n2"
    else if n1 = n2 then Set.empty
    else Set.add (VNat n1) (range (n1 + 1u) n2)

let rangeList (n1: uint) (n2: uint) : uint list =
    if n1 > n2 then
        failwith $"the first arg must not be greater than the second one: range {n1} {n2}"

    let rec loop n1 n2 =
        if n1 = n2 then [] else n1 :: (loop (n1 + 1u) n2) in

    loop n1 n2

let natMax = 5u

let powerSet (vs: 'v list) : Set<'v> list =
    let vss = List.map (fun v -> [ Some v; None ]) vs in

    List.fold
        (fun acc vs ->
            List.map
                (fun t ->
                    match t with
                    | Some v, s -> Set.add v s
                    | None, s -> s)
                (List.allPairs vs acc))
        []
        vss


let univ (m: CtorMap<'Ctor>) (t: Type) : Val<'Ctor> list =
    let rec univ t =
        match t with
        | TUnit -> [ VUnit ]
        | TNat -> List.map VNat (rangeList 0u natMax)
        | TBool -> [ VBool false; VBool true ]
        | TTuple(l, r) -> List.collect (fun (l, r) -> [ VTuple(l, r) ]) (List.allPairs (univ l) (univ r))
        | TSet t -> List.map VSet (powerSet (univ t))
        | TList t ->
            let vOpts = (None :: (List.map Some (univ t))) in

            List.map
                VList
                (List.fold
                    (fun acc vOpt ->
                        match vOpt with
                        | Some v -> List.map (fun vs -> v :: vs) acc
                        | None -> acc)
                    []
                    vOpts)
        | TMap(tk, tv) ->
            let vOptsList = List.map (fun v -> [ Some v; None ]) (univ tk) in

            List.map
                VMap
                (List.fold
                    (fun acc vs ->
                        List.collect
                            (fun t ->
                                match t with
                                | Some kv, m -> List.map (fun vv -> Map.add kv vv m) (univ tv)
                                | None, m -> [ m ])
                            (List.allPairs vs acc))
                    []
                    vOptsList)

        | TUnion _ ->
            List.map VUnion (List.collect (fun (c, (_, t)) -> List.map (fun v -> (c, v)) (univ t)) (Map.toList m))
        | TError -> [ VError ]
    univ t

let eval (m: CtorMap<'Ctor>) (env: Env<'Var, 'Ctor>) (expr: Expr<'Var, 'Ctor>) : Val<'Ctor> =
    let rec eval env expr =
        match expr with
        | Lit v -> v
        | Union(c, e) -> VUnion(c, eval env e)
        | UnionEq(_j, e1, e2) ->
            match eval env e1, eval env e2 with
            | VUnion(c1, v1), VUnion(c2, v2) -> VBool(c1 = c2 && v1 = v2)
            | _ -> VError
        | Throw -> VError
        | ErrorEq(e1, e2) ->
            match eval env e1, eval env e2 with
            | VError, VError -> VBool(true)
            | _ -> VError
        | If(e1, e2, e3) ->
            match eval env e1 with
            | VBool true -> eval env e2
            | VBool false -> eval env e3
            | _ -> VError
        | Match(e, m, d) ->
            match eval env e with
            | VUnion(c, v) ->
                match Map.tryFind c m with
                | Some(var, e1) ->
                    if Map.containsKey var env then
                        VError // NOTE: 変数シャドウはとりあえず落とす
                    else
                        eval (Map.add var v env) e1
                | None ->
                    match d with
                    | Some(Some var, e2) -> eval (Map.add var (VUnion(c, v)) env) e2
                    | Some(None, e2) -> eval env e2
                    | None -> VError
            | _ -> VError
        | VarRef v -> Map.find v env
        | BoolEq(e1, e2) ->
            match eval env e1, eval env e2 with
            | VBool b1, VBool b2 -> VBool(b1 = b2)
            | _ -> VError
        | BoolNot e ->
            match eval env e with
            | VBool b -> VBool(not b)
            | _ -> VError
        | BoolAnd(e1, e2) ->
            match eval env e1, eval env e2 with
            | VBool b1, VBool b2 -> VBool(b1 && b2)
            | _ -> VError
        | BoolOr(e1, e2) ->
            match eval env e1, eval env e2 with
            | VBool b1, VBool b2 -> VBool(b1 || b2)
            | _ -> VError
        | NatEq(e1, e2) ->
            match eval env e1, eval env e2 with
            | VNat n1, VNat n2 -> VBool(n1 = n2)
            | _ -> VError
        | NatLess(e1, e2) ->
            match eval env e1, eval env e2 with
            | VNat n1, VNat n2 -> VBool(n1 < n2)
            | _ -> VError
        | NatAdd(e1, e2) ->
            match eval env e1, eval env e2 with
            | VNat n1, VNat n2 -> VNat(n1 + n2)
            | _ -> VError
        | NatProd(e1, e2) ->
            match eval env e1, eval env e2 with
            | VNat n1, VNat n2 -> VNat(n1 * n2)
            | _ -> VError
        | NatSub(e1, e2) ->
            match eval env e1, eval env e2 with
            | VNat n1, VNat n2 -> VNat(if n1 < n2 then 0u else n1 - n2)
            | _ -> VError
        | Tuple(l, r) -> VTuple(eval env l, eval env r)
        | TupleEq(l, r) ->
            match eval env l, eval env r with
            | VTuple(l1, r1), VTuple(l2, r2) -> VBool(l1 = l2 && r1 = r2)
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
        | ListEq(e1, e2) ->
            match eval env e1, eval env e2 with
            | VList vs1, VList vs2 -> VBool(vs1 = vs2)
            | _ -> VError
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
        | SetEq(e1, e2) ->
            match eval env e1, eval env e2 with
            | VSet vs1, VSet vs2 -> VBool(vs1 = vs2)
            | _ -> VError
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
        | MapEq(e1, e2) ->
            match eval env e1, eval env e2 with
            | VMap vs1, VMap vs2 -> VBool(vs1 = vs2)
            | _ -> VError
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
        | Univ t -> VSet(Set.ofList (univ m t))
    eval env expr

let rec format (expr: Expr<'Var, 'Ctor>) : string =
    match expr with
    | Lit v -> Val.format v
    | Union(c, e) ->
        match c with
        | Ctor c' -> if e = Lit VUnit then $"{c'}" else $"({c'} {format e})"
        | CtorSome -> $"(Some {format e})"
        | CtorNone -> "None"
        | CtorLeft -> $"(Left {format e})"
        | CtorRight -> $"(Right {format e})"
    | UnionEq(name, e1, e2) -> $"({format e1} ={name} {format e2})"
    | Throw -> "throw"
    | ErrorEq(e1, e2) -> $"({format e1} =E {format e2})"
    | If(e1, e2, e3) -> $"(if {format e1} then {format e2} else {format e3})"
    | Match(e, cs, d) ->
        let sep = " | " in
        let cs' = List.map (fun (c, (v, e')) -> $"{c} {v} -> {format e'}") (Map.toList cs) in

        match d with
        | Some(v, e') -> $"(match {format e} with {String.concat sep cs'} | {v} -> {format e'})"
        | None -> $"(match {format e} with {String.concat sep cs'})"
    | VarRef v -> $"{v}"
    | BoolEq(e1, e2) -> $"({format e1} =B {format e2})"
    | BoolNot e -> $"(not ({format e}))"
    | BoolAnd(e1, e2) -> $"({format e1} && {format e2})"
    | BoolOr(e1, e2) -> $"({format e1} || {format e2})"
    | NatEq(e1, e2) -> $"({format e1} =N {format e2})"
    | NatLess(e1, e2) -> $"({format e1} < {format e2})"
    | NatAdd(e1, e2) -> $"({format e1} + {format e2})"
    | NatProd(e1, e2) -> $"({format e1} * {format e2})"
    | NatSub(e1, e2) -> $"({format e1} - {format e2})"
    | Tuple(l, r) -> $"({format l}, {format r})"
    | TupleEq(e1, e2) -> $"({format e1} =T {format e2})"
    | TupleFst e -> $"(fst {format e})"
    | TupleSnd e -> $"(snd {format e})"
    | ListEmpty -> "[]"
    | ListEq(e1, e2) -> $"({format e1} =L {format e2})"
    | ListCons(e1, e2) -> $"({format e1} :: {format e2})"
    | ListNth(e1, e2) -> $"(List.nth {format e1} {format e2})"
    | ListLen e -> $"(List.length {format e})"
    | SetEmpty -> "{}"
    | SetEq(e1, e2) -> $"({format e1} =S {format e2})"
    | SetRange(e1, e2) -> $"(Set.range {format e1} {format e2})"
    | SetInsert(e1, e2) -> $"(Set.add {format e1} {format e2})"
    | SetMem(e1, e2) -> $"(Set.contains {format e1} {format e2})"
    | SetFilter(v, e1, e2) -> $"(Set.filter (fun {v} -> {format e1}) {format e2})"
    | SetExists(v, e1, e2) -> $"(Set.exists (fun {v} -> {format e1}) {format e2})"
    | MapEmpty -> "Map.empty"
    | MapEq(e1, e2) -> $"({format e1} =M {format e2})"
    | MapAdd(k, v, m) -> $"(Map.add {format k} {format v} {format m})"
    | MapFindOpt(e1, e2) -> $"(Map.findOpt {format e1} {format e2})"
    | Univ t -> $"(univ::{Type.format t})"

let children (expr: Expr<'Var, 'Ctor>) : Expr<'Var, 'Ctor> list =
    match expr with
    | Lit _ -> []
    | Union(_, expr) -> [ expr ]
    | UnionEq(_, expr1, expr2) -> [ expr1; expr2 ]
    | Throw -> []
    | ErrorEq(expr1, expr2) -> [ expr1; expr2 ]
    | If(expr1, expr2, expr3) -> [ expr1; expr2; expr3 ]
    | Match(expr, vs, dOpt) ->
        let es = Map.fold (fun acc _ (_, expr) -> acc @ [ expr ]) [ expr ] vs in

        match dOpt with
        | Some(_, expr) -> es @ [ expr ]
        | None -> es
    | VarRef _ -> []
    | BoolEq(expr1, expr2) -> [ expr1; expr2 ]
    | BoolNot expr -> [ expr ]
    | BoolAnd(expr1, expr2) -> [ expr1; expr2 ]
    | BoolOr(expr1, expr2) -> [ expr1; expr2 ]
    | NatEq(expr1, expr2) -> [ expr1; expr2 ]
    | NatLess(expr1, expr2) -> [ expr1; expr2 ]
    | NatAdd(expr1, expr2) -> [ expr1; expr2 ]
    | NatProd(expr1, expr2) -> [ expr1; expr2 ]
    | NatSub(expr1, expr2) -> [ expr1; expr2 ]
    | Tuple(exprL, exprR) -> [ exprL; exprR ]
    | TupleEq(expr1, expr2) -> [ expr1; expr2 ]
    | TupleFst expr -> [ expr ]
    | TupleSnd expr -> [ expr ]
    | ListEmpty -> []
    | ListEq(expr1, expr2) -> [ expr1; expr2 ]
    | ListCons(expr1, expr2) -> [ expr1; expr2 ]
    | ListNth(expr1, expr2) -> [ expr1; expr2 ]
    | ListLen expr -> [ expr ]
    | SetEmpty -> []
    | SetEq(expr1, expr2) -> [ expr1; expr2 ]
    | SetRange(expr1, expr2) -> [ expr1; expr2 ]
    | SetInsert(expr1, expr2) -> [ expr1; expr2 ]
    | SetMem(expr1, expr2) -> [ expr1; expr2 ]
    | SetFilter(_, expr1, expr2) -> [ expr1; expr2 ]
    | SetExists(_, expr1, expr2) -> [ expr1; expr2 ]
    | MapEmpty -> []
    | MapEq(expr1, expr2) -> [ expr1; expr2 ]
    | MapAdd(expr1, expr2, expr3) -> [ expr1; expr2; expr3 ]
    | MapFindOpt(expr1, expr2) -> [ expr1; expr2 ]
    | Univ _ -> []

let rec descendant (expr: Expr<'Var, 'Ctor>) : Expr<'Var, 'Ctor> list =
    List.fold (fun acc expr -> acc @ descendant expr) [ expr ] (children expr)

let fold (f: 'State -> Expr<'Var, 'Ctor> -> 'State) (s0: 'State) (expr: Expr<'Var, 'Ctor>) : 'State =
    List.fold f s0 (descendant expr)
