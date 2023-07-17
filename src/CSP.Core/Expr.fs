module CSP.Core.Expr

open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.LineNum
open CSP.Core.Type
open CSP.Core.Val
open CSP.Core.Env
open CSP.Core.Var

type Expr =
    | LitUnit of LineNum
    | LitTrue of LineNum
    | LitFalse of LineNum
    | LitNat of uint * LineNum
    | LitEmpty of Type * LineNum
    | LitError of LineNum
    | Union of Ctor * Expr * LineNum
    | Throw of LineNum
    | If of Expr * Expr * Expr * LineNum
    | Match of Expr * Map<Ctor, Var * Expr> * (Var option * Expr) option * LineNum
    | VarRef of Var * LineNum
    | Eq of Type * Expr * Expr * LineNum
    | Not of Expr * LineNum
    | Less of Type * Expr * Expr * LineNum
    | Plus of Type * Expr * Expr * LineNum
    | Minus of Type * Expr * Expr * LineNum
    | Times of Type * Expr * Expr * LineNum
    | Size of Type * Expr * LineNum
    | Filter of Type * Var * Expr * Expr * LineNum
    | Exists of Type * Var * Expr * Expr * LineNum
    | Tuple of Expr * Expr * LineNum
    | TupleFst of Expr * LineNum
    | TupleSnd of Expr * LineNum
    | ListCons of Expr * Expr * LineNum
    | ListNth of Expr * Expr * LineNum
    | SetRange of Expr * Expr * LineNum
    | SetInsert of Expr * Expr * LineNum
    | SetMem of Expr * Expr * LineNum
    | MapAdd of Expr * Expr * Expr * LineNum
    | MapFindOpt of Expr * Expr * LineNum
    | Univ of Type * LineNum

let rec rangeVNat (n1: uint32) (n2: uint32) : Set<Val> = Set.map VNat (Range.ofSet n1 n2)

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


let univ (m: CtorMap) (t: Type) : Val list =
    let rec univ t =
        match t with
        | TVar _ -> failwith "cannot use type vars as a parameter of univ"
        | TUnit -> [ VUnit ]
        | TNat -> List.map VNat (Range.ofList 0u natMax)
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

let eval (m: CtorMap) (env: Env) (expr: Expr) : Val =
    let rec eval env expr =
        match expr with
        | LitUnit _ -> VUnit
        | LitTrue _ -> VBool(true)
        | LitFalse _ -> VBool(false)
        | LitNat(n, _) -> VNat(n)
        | LitEmpty(t, _) ->
            match t with
            | TSet _ -> VSet Set.empty
            | TList _ -> VList List.empty
            | TMap _ -> VMap Map.empty
            | _ -> VError
        | LitError _ -> VError
        | Union(c, e, _) -> VUnion(c, eval env e)
        | Throw _ -> VError
        | If(e1, e2, e3, _) ->
            match eval env e1 with
            | VBool true -> eval env e2
            | VBool false -> eval env e3
            | _ -> VError
        | Match(e, m, d, _) ->
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
        | Eq(t, e1, e2, _) ->
            match t, eval env e1, eval env e2 with
            | TUnit, VUnit, VUnit -> VBool(true)
            | TNat, VNat n1, VNat n2 -> VBool(n1 = n2)
            | TBool, VBool b1, VBool b2 -> VBool(b1 = b2)
            | TTuple _, VTuple(vL1, vR1), VTuple(vL2, vR2) -> VBool((vL1, vR1) = (vL2, vR2))
            | TSet _, VSet s1, VSet s2 -> VBool(s1 = s2)
            | TList _, VList vs1, VList vs2 -> VBool(vs1 = vs2)
            | TMap _, VMap m1, VMap m2 -> VBool(m1 = m2)
            | TUnion _, VUnion(ctor1, v1), VUnion(ctor2, v2) -> VBool(ctor1 = ctor2 && v1 = v2)
            | TError, VError, VError -> VBool(true)
            | _, _, _ -> VError
        | Less(t, e1, e2, _) ->
            match t, eval env e1, eval env e2 with
            | TNat, VNat n1, VNat n2 -> VBool(n1 < n2)
            | TBool, VBool b1, VBool b2 -> VBool(b2 && not b1)
            | TSet _, VSet s1, VSet s2 -> VBool(Set.isSubset s1 s2)
            | _, _, _ -> VError
        | Size(t, expr, _) ->
            match t, eval env expr with
            | TSet _, VSet s -> VNat(uint (Set.count s))
            | TList _, VList vs -> VNat(uint (List.length vs))
            | TMap _, VMap m -> VNat(uint (Map.count m))
            | _ -> VError
        | VarRef(v, _) -> Map.find v env
        | Not(e, _) ->
            match eval env e with
            | VBool b -> VBool(not b)
            | _ -> VError
        | Plus(t, e1, e2, _) ->
            match t, eval env e1, eval env e2 with
            | TBool, VBool n1, VBool n2 -> VBool(n1 || n2)
            | TNat, VNat n1, VNat n2 -> VNat(n1 + n2)
            | TSet _, VSet n1, VSet n2 -> VSet(Set.union n1 n2)
            | TList _, VList n1, VList n2 -> VList(List.append n1 n2)
            | _ -> VError
        | Times(t, e1, e2, _) ->
            match t, eval env e1, eval env e2 with
            | TBool, VBool n1, VBool n2 -> VBool(n1 && n2)
            | TNat, VNat n1, VNat n2 -> VNat(n1 * n2)
            | TSet _, VSet n1, VSet n2 -> VSet(Set.intersect n1 n2)
            | _ -> VError
        | Minus(t, e1, e2, _) ->
            match t, eval env e1, eval env e2 with
            | TBool, VBool b1, VBool b2 -> VBool(not b2 || b1)
            | TNat, VNat n1, VNat n2 -> VNat(if n1 < n2 then 0u else n1 - n2)
            | TSet _, VSet s1, VSet s2 -> VSet(Set.difference s1 s2)
            | _ -> VError
        | Tuple(l, r, _) -> VTuple(eval env l, eval env r)
        | TupleFst(e, _) ->
            match eval env e with
            | VTuple(l, _) -> l
            | _ -> VError
        | TupleSnd(e, _) ->
            match eval env e with
            | VTuple(_, r) -> r
            | _ -> VError
        | ListCons(e1, e2, _) ->
            match eval env e1, eval env e2 with
            | v, VList vs -> VList(v :: vs)
            | _ -> VError
        | ListNth(e1, e2, _) ->
            match eval env e1, eval env e2 with
            | VNat n, VList vs -> List.item (Checked.int n) vs
            | _ -> VError
        | SetRange(e1, e2, _) ->
            match eval env e1, eval env e2 with
            | VNat n1, VNat n2 when n1 <= n2 -> VSet(Set.map VNat (Range.ofSet n1 n2))
            | _ -> VError
        | SetInsert(e1, e2, _) ->
            match eval env e1, eval env e2 with
            | v, VSet vs -> VSet(Set.add v vs)
            | _ -> VError
        | SetMem(e1, e2, _) ->
            match eval env e1, eval env e2 with
            | v, VSet vs -> VBool(Set.contains v vs)
            | _ -> VError
        | Filter(_, var, e1, e2, _) ->
            match eval env e2 with
            | VSet s ->
                (Set.fold
                    (fun acc v ->
                        match acc with
                        | VSet vsAcc ->
                            match eval (Map.add var v env) e1 with
                            | VBool b -> VSet(if b then Set.add v vsAcc else s)
                            | _ -> VError
                        | _ -> VError)
                    (VSet Set.empty)
                    s)
            | VList vs ->
                (List.fold
                    (fun acc v ->
                        match acc with
                        | VList vsAcc ->
                            match eval (Map.add var v env) e1 with
                            | VBool b -> VList(if b then v :: vsAcc else vs)
                            | _ -> VError
                        | _ -> VError)
                    (VList List.empty)
                    vs)
            | _ -> VError
        | Exists(_, var, e1, e2, _) ->
            match eval env e2 with
            | VSet s ->
                (Set.fold
                    (fun acc v ->
                        match acc with
                        | VBool true -> VBool true
                        | VBool false ->
                            match eval (Map.add var v env) e1 with
                            | VBool b -> VBool b
                            | _ -> VError
                        | _ -> VError)
                    (VBool false)
                    s)
            | VList vs ->
                (List.fold
                    (fun acc v ->
                        match acc with
                        | VBool true -> VBool true
                        | VBool false ->
                            match eval (Map.add var v env) e1 with
                            | VBool b -> VBool b
                            | _ -> VError
                        | _ -> VError)
                    (VBool false)
                    vs)
            | _ -> VError
        | MapAdd(e1, e2, e3, _) ->
            match eval env e1, eval env e2, eval env e3 with
            | k, v, VMap m -> VMap(Map.add k v m)
            | _ -> VError
        | MapFindOpt(e1, e2, _) ->
            match eval env e1, eval env e2 with
            | k, VMap m ->
                match Map.tryFind k m with
                | Some v -> VUnion(Ctor "Some", v)
                | None -> VUnion(Ctor "None", VUnit)
            | _ -> VError
        | Univ(t, _) -> VSet(Set.ofList (univ m t))

    eval env expr

let format (pretty: bool) (expr: Expr) : string =
    let rec format indent expr =
        match expr with
        | LitUnit _ -> "()"
        | LitTrue _ -> "true"
        | LitFalse _ -> "false"
        | LitNat(n, _) -> $"%d{n}"
        | LitEmpty(t, _) -> $"{Type.format t}.empty"
        | LitError _ -> "ERROR"
        | Union(ctor, e, _) ->
            match e with
            | LitUnit _ -> $"{Ctor.format ctor}"
            | _ -> $"({Ctor.format ctor} {format e})"
        | Throw _ -> "throw"
        | If(e1, e2, e3, _) -> $"(if {format e1} then {format e2} else {format e3})"
        | Match(e, cs, d, _) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, e')) -> $"{c} {v} -> {format e'}") (Map.toList cs) in

            match d with
            | Some(v, e') -> $"(match {format e} with {String.concat sep cs'} | {v} -> {format e'})"
            | None -> $"(match {format e} with {String.concat sep cs'})"
        | VarRef(v, _) -> $"{v}"
        | Not(expr, _) -> $"(not {format expr})"
        | Eq(t, expr1, expr2, _) -> $"{Type.format t}.equal {format expr1} {format expr2}"
        | Less(t, expr1, expr2, _) -> $"{Type.format t}.less {format expr1} {format expr2}"
        | Plus(t, expr1, expr2, _) -> $"{Type.format t}.plus {format expr1} {format expr2}"
        | Minus(t, expr1, expr2, _) -> $"{Type.format t}.minus {format expr1} {format expr2}"
        | Times(t, expr1, expr2, _) -> $"{Type.format t}.times {format expr1} {format expr2}"
        | Size(t, expr, _) -> $"({Type.format t}.count {format expr})"
        | Exists(t, var, expr1, expr2, _) -> $"{Type.format t}.exists (fun {var} -> {format expr1}) {format expr2}"
        | Filter(t, var, expr1, expr2, _) -> $"{Type.format t}.filter (fun {var} -> {format expr1}) {format expr2}"
        | Tuple(l, r, _) -> $"({format l}, {format r})"
        | TupleFst(e, _) -> $"(fst {format e})"
        | TupleSnd(e, _) -> $"(snd {format e})"
        | ListCons(e1, e2, _) -> $"({format e1} :: {format e2})"
        | ListNth(e1, e2, _) -> $"(List.nth {format e1} {format e2})"
        | SetRange(e1, e2, _) -> $"(Set.range {format e1} {format e2})"
        | SetInsert(e1, e2, _) -> $"(Set.add {format e1} {format e2})"
        | SetMem(e1, e2, _) -> $"(Set.contains {format e1} {format e2})"
        | MapAdd(k, v, m, _) -> $"(Map.add {format k} {format v} {format m})"
        | MapFindOpt(e1, e2, _) -> $"(Map.findOpt {format e1} {format e2})"
        | Univ(t, _) -> $"(univ::{Type.format t})"
    format expr

let line (expr: Expr) : LineNum =
    match expr with
    | LitUnit line -> line
    | LitTrue line -> line
    | LitFalse line -> line
    | LitNat(_, line) -> line
    | LitError line -> line
    | LitEmpty(_, line) -> line
    | Union(_, _, line) -> line
    | Throw line -> line
    | If(_, _, _, line) -> line
    | Match(_, _, _, line) -> line
    | VarRef(_, line) -> line
    | Not(_, line) -> line
    | Plus(_, _, _, line) -> line
    | Times(_, _, _, line) -> line
    | Minus(_, _, _, line) -> line
    | Less(_, _, _, line) -> line
    | Eq(_, _, _, line) -> line
    | Size(_, _, line) -> line
    | Filter(_, _, _, _, line) -> line
    | Exists(_, _, _, _, line) -> line
    | Tuple(_, _, line) -> line
    | TupleFst(_, line) -> line
    | TupleSnd(_, line) -> line
    | ListCons(_, _, line) -> line
    | ListNth(_, _, line) -> line
    | SetRange(_, _, line) -> line
    | SetInsert(_, _, line) -> line
    | SetMem(_, _, line) -> line
    | MapAdd(_, _, _, line) -> line
    | MapFindOpt(_, _, line) -> line
    | Univ(_, line) -> line

let children (expr: Expr) : Expr list =
    match expr with
    | LitUnit _ -> []
    | LitTrue _ -> []
    | LitFalse _ -> []
    | LitNat _ -> []
    | LitError _ -> []
    | LitEmpty _ -> []
    | Union(_, expr, _) -> [ expr ]
    | Throw _ -> []
    | VarRef _ -> []
    | If(expr1, expr2, expr3, _) -> [ expr1; expr2; expr3 ]
    | Match(expr, vs, dOpt, _) ->
        let es = Map.fold (fun acc _ (_, expr) -> acc @ [ expr ]) [ expr ] vs in

        match dOpt with
        | Some(_, expr) -> es @ [ expr ]
        | None -> es
    | Not(expr, _) -> [ expr ]
    | Eq(_, expr1, expr2, _) -> [ expr1; expr2 ]
    | Less(_, expr1, expr2, _) -> [ expr1; expr2 ]
    | Plus(_, expr1, expr2, _) -> [ expr1; expr2 ]
    | Minus(_, expr1, expr2, _) -> [ expr1; expr2 ]
    | Times(_, expr1, expr2, _) -> [ expr1; expr2 ]
    | Tuple(exprL, exprR, _) -> [ exprL; exprR ]
    | TupleFst(expr, _) -> [ expr ]
    | TupleSnd(expr, _) -> [ expr ]
    | Size(_, expr, _) -> [ expr ]
    | Filter(_, _, _, expr, _) -> [ expr ]
    | Exists(_, _, _, expr, _) -> [ expr ]
    | ListCons(expr1, expr2, _) -> [ expr1; expr2 ]
    | ListNth(expr1, expr2, _) -> [ expr1; expr2 ]
    | SetRange(expr1, expr2, _) -> [ expr1; expr2 ]
    | SetInsert(expr1, expr2, _) -> [ expr1; expr2 ]
    | SetMem(expr1, expr2, _) -> [ expr1; expr2 ]
    | MapAdd(expr1, expr2, expr3, _) -> [ expr1; expr2; expr3 ]
    | MapFindOpt(expr1, expr2, _) -> [ expr1; expr2 ]
    | Univ _ -> []

let dfs (visit: Expr -> Unit) (expr: Expr) : Unit =
    Search.dfs (fun expr _ -> visit expr) 10000 (fun expr -> List.map (fun expr -> ((), expr)) (children expr)) id expr

let bfs (visit: Expr -> Unit) (expr: Expr) : Unit =
    Search.bfs (fun expr _ -> visit expr) 10000 (fun expr -> List.map (fun expr -> ((), expr)) (children expr)) id expr
