module CSP.Core.Expr

open CSP.Core.Indent
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.LineNum
open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.Val
open CSP.Core.Env
open CSP.Core.Var

type Expr =
    | LitUnit of TypeCstr option * LineNum
    | LitTrue of TypeCstr option * LineNum
    | LitFalse of TypeCstr option * LineNum
    | LitNat of uint * TypeCstr option * LineNum
    | LitEmpty of Type * TypeCstr option * LineNum
    | LitError of TypeCstr option * LineNum
    | Union of Ctor * Expr * TypeCstr option * LineNum
    | Throw of TypeCstr option * LineNum
    | If of Expr * Expr * Expr * TypeCstr option * LineNum
    | Match of Expr * Map<Ctor, Var option * Expr> * (Var option * Expr) option * TypeCstr option * LineNum
    | VarRef of Var * TypeCstr option * LineNum
    | Eq of Type * Expr * Expr * TypeCstr option * LineNum
    | Not of Expr * TypeCstr option * LineNum
    | Less of Type * Expr * Expr * TypeCstr option * LineNum
    | Plus of Type * Expr * Expr * TypeCstr option * LineNum
    | Minus of Type * Expr * Expr * TypeCstr option * LineNum
    | Times of Type * Expr * Expr * TypeCstr option * LineNum
    | Size of Type * Expr * TypeCstr option * LineNum
    | Filter of Type * Var * Expr * Expr * TypeCstr option * LineNum
    | Exists of Type * Var * Expr * Expr * TypeCstr option * LineNum
    | Tuple of Expr * Expr * TypeCstr option * LineNum
    | TupleFst of Expr * TypeCstr option * LineNum
    | TupleSnd of Expr * TypeCstr option * LineNum
    | ListCons of Expr * Expr * TypeCstr option * LineNum
    | ListNth of Expr * Expr * TypeCstr option * LineNum
    | SetRange of Expr * Expr * TypeCstr option * LineNum
    | SetInsert of Expr * Expr * TypeCstr option * LineNum
    | SetMem of Expr * Expr * TypeCstr option * LineNum
    | MapAdd of Expr * Expr * Expr * TypeCstr option * LineNum
    | MapFindOpt of Expr * Expr * TypeCstr option * LineNum
    | Univ of Type * TypeCstr option * LineNum

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
        | LitNat(n, _, _) -> VNat(n)
        | LitEmpty(t, _, _) ->
            match t with
            | TSet _ -> VSet Set.empty
            | TList _ -> VList List.empty
            | TMap _ -> VMap Map.empty
            | _ -> VError
        | LitError _ -> VError
        | Union(c, e, _, _) -> VUnion(c, eval env e)
        | Throw _ -> VError
        | If(e1, e2, e3, _, _) ->
            match eval env e1 with
            | VBool true -> eval env e2
            | VBool false -> eval env e3
            | _ -> VError
        | Match(e, m, d, _, _) ->
            match eval env e with
            | VUnion(c, v) ->
                match Map.tryFind c m with
                | Some(Some(var), e1) ->
                    if Map.containsKey var env then
                        VError // NOTE: 変数シャドウはとりあえず落とす
                    else
                        eval (Map.add var v env) e1
                | Some(None, e1) -> eval env e1
                | None ->
                    match d with
                    | Some(Some var, e2) -> eval (Map.add var (VUnion(c, v)) env) e2
                    | Some(None, e2) -> eval env e2
                    | None -> VError
            | _ -> VError
        | Eq(t, e1, e2, _, _) ->
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
        | Less(t, e1, e2, _, _) ->
            match t, eval env e1, eval env e2 with
            | TNat, VNat n1, VNat n2 -> VBool(n1 < n2)
            | TBool, VBool b1, VBool b2 -> VBool(b2 && not b1)
            | TSet _, VSet s1, VSet s2 -> VBool(Set.isSubset s1 s2)
            | _, _, _ -> VError
        | Size(t, expr, _, _) ->
            match t, eval env expr with
            | TSet _, VSet s -> VNat(uint (Set.count s))
            | TList _, VList vs -> VNat(uint (List.length vs))
            | TMap _, VMap m -> VNat(uint (Map.count m))
            | _ -> VError
        | VarRef(v, _, _) -> Map.find v env
        | Not(e, _, _) ->
            match eval env e with
            | VBool b -> VBool(not b)
            | _ -> VError
        | Plus(t, e1, e2, _, _) ->
            match t, eval env e1, eval env e2 with
            | TBool, VBool n1, VBool n2 -> VBool(n1 || n2)
            | TNat, VNat n1, VNat n2 -> VNat(n1 + n2)
            | TSet _, VSet n1, VSet n2 -> VSet(Set.union n1 n2)
            | TList _, VList n1, VList n2 -> VList(List.append n1 n2)
            | _ -> VError
        | Times(t, e1, e2, _, _) ->
            match t, eval env e1, eval env e2 with
            | TBool, VBool n1, VBool n2 -> VBool(n1 && n2)
            | TNat, VNat n1, VNat n2 -> VNat(n1 * n2)
            | TSet _, VSet n1, VSet n2 -> VSet(Set.intersect n1 n2)
            | _ -> VError
        | Minus(t, e1, e2, _, _) ->
            match t, eval env e1, eval env e2 with
            | TBool, VBool b1, VBool b2 -> VBool(not b2 || b1)
            | TNat, VNat n1, VNat n2 -> VNat(if n1 < n2 then 0u else n1 - n2)
            | TSet _, VSet s1, VSet s2 -> VSet(Set.difference s1 s2)
            | _ -> VError
        | Tuple(l, r, _, _) -> VTuple(eval env l, eval env r)
        | TupleFst(e, _, _) ->
            match eval env e with
            | VTuple(l, _) -> l
            | _ -> VError
        | TupleSnd(e, _, _) ->
            match eval env e with
            | VTuple(_, r) -> r
            | _ -> VError
        | ListCons(e1, e2, _, _) ->
            match eval env e1, eval env e2 with
            | v, VList vs -> VList(v :: vs)
            | _ -> VError
        | ListNth(e1, e2, _, _) ->
            match eval env e1, eval env e2 with
            | VNat n, VList vs -> List.item (Checked.int n) vs
            | _ -> VError
        | SetRange(e1, e2, _, _) ->
            match eval env e1, eval env e2 with
            | VNat n1, VNat n2 when n1 <= n2 -> VSet(Set.map VNat (Range.ofSet n1 n2))
            | _ -> VError
        | SetInsert(e1, e2, _, _) ->
            match eval env e1, eval env e2 with
            | v, VSet vs -> VSet(Set.add v vs)
            | _ -> VError
        | SetMem(e1, e2, _, _) ->
            match eval env e1, eval env e2 with
            | v, VSet vs -> VBool(Set.contains v vs)
            | _ -> VError
        | Filter(_, var, e1, e2, _, _) ->
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
        | Exists(_, var, e1, e2, _, _) ->
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
        | MapAdd(e1, e2, e3, _, _) ->
            match eval env e1, eval env e2, eval env e3 with
            | k, v, VMap m -> VMap(Map.add k v m)
            | _ -> VError
        | MapFindOpt(e1, e2, _, _) ->
            match eval env e1, eval env e2 with
            | k, VMap m ->
                match Map.tryFind k m with
                | Some v -> VUnion(Ctor "Some", v)
                | None -> VUnion(Ctor "None", VUnit)
            | _ -> VError
        | Univ(t, _, _) -> VSet(Set.ofList (univ m t))

    eval env expr

let typeAnn (s: string) (tcOpt: TypeCstr option) =
    match tcOpt with
    | Some tc -> $"({s}::{TypeCstr.format tc})"
    | None -> s

let format (expr: Expr) : string =
    let rec format indent expr =
        let indent1 = indent + 1u in
        let indent2 = indent + 2u in
        let indent3 = indent + 3u in

        match expr with
        | LitUnit(tcOpt, _) -> typeAnn "()" tcOpt
        | LitTrue(tcOpt, _) -> typeAnn "true" tcOpt
        | LitFalse(tcOpt, _) -> typeAnn "false" tcOpt
        | LitNat(n, tcOpt, _) -> typeAnn $"%d{n}" tcOpt
        | LitEmpty(t, tcOpt, _) -> typeAnn $"%s{Type.format t}.empty" tcOpt
        | LitError(tcOpt, _) -> typeAnn "error" tcOpt
        | Union(ctor, e, tcOpt, _) ->
            match e with
            | LitUnit _ -> typeAnn $"%s{Ctor.format ctor}" tcOpt
            | _ ->
                typeAnn
                    $"""(%s{Ctor.format ctor}
{render indent} %s{format indent1 e})"""
                    tcOpt
        | Throw(tcOpt, _) -> typeAnn "throw" tcOpt
        | If(e1, e2, e3, tcOpt, _) ->
            typeAnn
                $"""(if %s{format indent1 e1}
{render indent}then
{render indent1}%s{format indent2 e2}
{render indent}else
{render indent1}%s{format indent2 e3})"""
                tcOpt
        | Match(e, cs, d, tcOpt, _) ->
            let lines =
                String.concat
                    ""
                    (List.map
                        (fun (ctor, (varOpt, e')) ->
                            match varOpt with
                            | Some var -> $"\n{render indent}| %s{Ctor.format ctor} %s{Var.format var} ->\n{render indent1}%s{format indent2 e'}"
                            | None -> $"\n{render indent}| %s{Ctor.format ctor} _ ->\n{render indent1}%s{format indent2 e'}")
                        (Map.toList cs)) in

            match d with
            | Some(Some v, e') ->
                typeAnn
                    $"""(match %s{format indent1 e} with%s{lines}
{render indent}| %s{Var.format v} ->
{render indent1}%s{format indent2 e'})"""
                    tcOpt
            | Some(None, e') ->
                typeAnn
                    $"""(match %s{format indent1 e} with{lines}
{render indent}| _ ->
{render indent1}%s{format indent2 e'})"""
                    tcOpt
            | None -> typeAnn $"(match %s{format indent1 e} with%s{lines})" tcOpt
        | VarRef(v, tcOpt, _) -> typeAnn $"%s{Var.format v}" tcOpt
        | Not(expr, tcOpt, _) -> typeAnn $"""(not %s{format indent2 expr})""" tcOpt
        | Eq(t, expr1, expr2, tcOpt, _) ->
            typeAnn
                $"""(%s{Type.format t}.equal
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                tcOpt
        | Less(t, expr1, expr2, tcOpt, _) ->
            typeAnn
                $"""(%s{Type.format t}.less
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                tcOpt
        | Plus(t, expr1, expr2, tcOpt, _) ->
            typeAnn
                $"""(%s{Type.format t}.plus
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                tcOpt
        | Minus(t, expr1, expr2, tcOpt, _) ->
            typeAnn
                $"""(%s{Type.format t}.minus
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                tcOpt
        | Times(t, expr1, expr2, tcOpt, _) ->
            typeAnn
                $"""(%s{Type.format t}.times
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                tcOpt
        | Size(t, expr, tcOpt, _) -> typeAnn $"""(%s{Type.format t}.count %s{format indent2 expr})""" tcOpt
        | Exists(t, var, expr1, expr2, tcOpt, _) ->
            typeAnn
                $"""(%s{Type.format t}.exists
{render indent1}(fun %s{Var.format var} ->
{render indent2}%s{format indent3 expr1})
{render indent1}%s{format indent2 expr2})"""
                tcOpt
        | Filter(t, var, expr1, expr2, tcOpt, _) ->
            typeAnn
                $"(%s{Type.format t}.filter
{render indent1}(fun %s{Var.format var} ->
{render indent2}%s{format indent3 expr1})
{render indent1}%s{format indent2 expr2})"
                tcOpt
        | Tuple(l, r, tcOpt, _) ->
            typeAnn
                $"(
{render indent1}%s{format indent2 l},
{render indent1}%s{format indent2 r})"
                tcOpt
        | TupleFst(e, tcOpt, _) -> typeAnn $"(fst %s{format indent1 e})" tcOpt
        | TupleSnd(e, tcOpt, _) -> typeAnn $"(snd %s{format indent1 e})" tcOpt
        | ListCons(e1, e2, tcOpt, _) ->
            typeAnn
                $"""(List.cons
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                tcOpt
        | ListNth(e1, e2, tcOpt, _) ->
            typeAnn
                $"""(List.nth
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                tcOpt
        | SetRange(e1, e2, tcOpt, _) ->
            typeAnn
                $"""(Set.range
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                tcOpt
        | SetInsert(e1, e2, tcOpt, _) ->
            typeAnn
                $"""(Set.insert
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                tcOpt
        | SetMem(e1, e2, tcOpt, _) ->
            typeAnn
                $"""(Set.contains
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                tcOpt
        | MapAdd(k, v, m, tcOpt, _) ->
            typeAnn
                $"""(Map.add
{render indent1}%s{format indent2 k}
{render indent1}%s{format indent2 v}
{render indent1}%s{format indent2 m})"""
                tcOpt
        | MapFindOpt(e1, e2, tcOpt, _) ->
            typeAnn
                $"""(Map.findOpt
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                tcOpt
        | Univ(t, tcOpt, _) -> typeAnn $"(univ::%s{Type.format t})" tcOpt

    format 0u expr

let line (expr: Expr) : LineNum =
    match expr with
    | LitUnit(_, line) -> line
    | LitTrue(_, line) -> line
    | LitFalse(_, line) -> line
    | LitNat(_, _, line) -> line
    | LitError(_, line) -> line
    | LitEmpty(_, _, line) -> line
    | Union(_, _, _, line) -> line
    | Throw(_, line) -> line
    | If(_, _, _, _, line) -> line
    | Match(_, _, _, _, line) -> line
    | VarRef(_, _, line) -> line
    | Not(_, _, line) -> line
    | Plus(_, _, _, _, line) -> line
    | Times(_, _, _, _, line) -> line
    | Minus(_, _, _, _, line) -> line
    | Less(_, _, _, _, line) -> line
    | Eq(_, _, _, _, line) -> line
    | Size(_, _, _, line) -> line
    | Filter(_, _, _, _, _, line) -> line
    | Exists(_, _, _, _, _, line) -> line
    | Tuple(_, _, _, line) -> line
    | TupleFst(_, _, line) -> line
    | TupleSnd(_, _, line) -> line
    | ListCons(_, _, _, line) -> line
    | ListNth(_, _, _, line) -> line
    | SetRange(_, _, _, line) -> line
    | SetInsert(_, _, _, line) -> line
    | SetMem(_, _, _, line) -> line
    | MapAdd(_, _, _, _, line) -> line
    | MapFindOpt(_, _, _, line) -> line
    | Univ(_, _, line) -> line

let typeCstr (expr: Expr) : TypeCstr =
    match expr with
    | LitUnit(Some tc, _) -> tc
    | LitTrue(Some tc, _) -> tc
    | LitFalse(Some tc, _) -> tc
    | LitNat(_, Some tc, _) -> tc
    | LitError(Some tc, _) -> tc
    | LitEmpty(_, Some tc, _) -> tc
    | Union(_, _, Some tc, _) -> tc
    | Throw(Some tc, _) -> tc
    | If(_, _, _, Some tc, _) -> tc
    | Match(_, _, _, Some tc, _) -> tc
    | VarRef(_, Some tc, _) -> tc
    | Not(_, Some tc, _) -> tc
    | Plus(_, _, _, Some tc, _) -> tc
    | Times(_, _, _, Some tc, _) -> tc
    | Minus(_, _, _, Some tc, _) -> tc
    | Less(_, _, _, Some tc, _) -> tc
    | Eq(_, _, _, Some tc, _) -> tc
    | Size(_, _, Some tc, _) -> tc
    | Filter(_, _, _, _, Some tc, _) -> tc
    | Exists(_, _, _, _, Some tc, _) -> tc
    | Tuple(_, _, Some tc, _) -> tc
    | TupleFst(_, Some tc, _) -> tc
    | TupleSnd(_, Some tc, _) -> tc
    | ListCons(_, _, Some tc, _) -> tc
    | ListNth(_, _, Some tc, _) -> tc
    | SetRange(_, _, Some tc, _) -> tc
    | SetInsert(_, _, Some tc, _) -> tc
    | SetMem(_, _, Some tc, _) -> tc
    | MapAdd(_, _, _, Some tc, _) -> tc
    | MapFindOpt(_, _, Some tc, _) -> tc
    | Univ(_, Some tc, _) -> tc
    | _ -> failwithf $"not typed {format expr}"

let children (expr: Expr) : Expr list =
    match expr with
    | LitUnit _ -> []
    | LitTrue _ -> []
    | LitFalse _ -> []
    | LitNat _ -> []
    | LitError _ -> []
    | LitEmpty _ -> []
    | Union(_, expr, _, _) -> [ expr ]
    | Throw _ -> []
    | VarRef _ -> []
    | If(expr1, expr2, expr3, _, _) -> [ expr1; expr2; expr3 ]
    | Match(expr, vs, dOpt, _, _) ->
        let es = Map.fold (fun acc _ (_, expr) -> acc @ [ expr ]) [ expr ] vs in

        match dOpt with
        | Some(_, expr) -> es @ [ expr ]
        | None -> es
    | Not(expr, _, _) -> [ expr ]
    | Eq(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Less(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Plus(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Minus(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Times(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Tuple(exprL, exprR, _, _) -> [ exprL; exprR ]
    | TupleFst(expr, _, _) -> [ expr ]
    | TupleSnd(expr, _, _) -> [ expr ]
    | Size(_, expr, _, _) -> [ expr ]
    | Filter(_, _, _, expr, _, _) -> [ expr ]
    | Exists(_, _, _, expr, _, _) -> [ expr ]
    | ListCons(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | ListNth(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | SetRange(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | SetInsert(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | SetMem(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | MapAdd(expr1, expr2, expr3, _, _) -> [ expr1; expr2; expr3 ]
    | MapFindOpt(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Univ _ -> []

type typeToGetMaxValue = int32

let dfs (visit: Expr -> Unit) (expr: Expr) : Unit =
    Search.dfs
        (fun expr _ -> visit expr)
        typeToGetMaxValue.MaxValue
        (fun expr -> List.map (fun expr -> ((), expr)) (children expr))
        id
        expr

let bfs (visit: Expr -> Unit) (expr: Expr) : Unit =
    Search.bfs
        (fun expr _ -> visit expr)
        typeToGetMaxValue.MaxValue
        (fun expr -> List.map (fun expr -> ((), expr)) (children expr))
        id
        expr

let mapTypeCstr (f: TypeCstr option -> TypeCstr option) (expr: Expr) =
    let rec mapTypeCstr expr =
        match expr with
        | LitUnit(tcOpt, line) -> LitUnit(f tcOpt, line)
        | LitTrue(tcOpt, line) -> LitTrue(f tcOpt, line)
        | LitFalse(tcOpt, line) -> LitFalse(f tcOpt, line)
        | LitNat(n, tcOpt, line) -> LitNat(n, f tcOpt, line)
        | LitError(tcOpt, line) -> LitError(f tcOpt, line)
        | LitEmpty(t, tcOpt, line) -> LitEmpty(t, f tcOpt, line)
        | Union(ctor, exprVal, tcOpt, line) -> Union(ctor, mapTypeCstr exprVal, f tcOpt, line)
        | Throw(tcOpt, line) -> Throw(f tcOpt, line)
        | If(exprCond, exprThen, exprFalse, tcOpt, line) ->
            If(mapTypeCstr exprCond, mapTypeCstr exprThen, mapTypeCstr exprFalse, f tcOpt, line)
        | Match(exprUnion, exprMap, exprDef, tcOpt, line) ->
            Match(
                mapTypeCstr exprUnion,
                Map.map (fun _ (var, expr) -> (var, mapTypeCstr expr)) exprMap,
                Option.map (fun (var, expr) -> (var, mapTypeCstr expr)) exprDef,
                f tcOpt,
                line
            )
        | VarRef(var, tcOpt, line) -> VarRef(var, f tcOpt, line)
        | Not(expr, tcOpt, line) -> Not(mapTypeCstr expr, f tcOpt, line)
        | Plus(t, expr1, expr2, tcOpt, line) -> Plus(t, mapTypeCstr expr1, mapTypeCstr expr2, f tcOpt, line)
        | Times(t, expr1, expr2, tcOpt, line) -> Times(t, mapTypeCstr expr1, mapTypeCstr expr2, f tcOpt, line)
        | Minus(t, expr1, expr2, tcOpt, line) -> Minus(t, mapTypeCstr expr1, mapTypeCstr expr2, f tcOpt, line)
        | Less(t, expr1, expr2, tcOpt, line) -> Less(t, mapTypeCstr expr1, mapTypeCstr expr2, f tcOpt, line)
        | Eq(t, expr1, expr2, tcOpt, line) -> Eq(t, mapTypeCstr expr1, mapTypeCstr expr2, f tcOpt, line)
        | Size(t, expr, tcOpt, line) -> Size(t, mapTypeCstr expr, f tcOpt, line)
        | Filter(t, var, exprCond, expr, tcOpt, line) ->
            Filter(t, var, mapTypeCstr exprCond, mapTypeCstr expr, f tcOpt, line)
        | Exists(t, var, exprCond, expr, tcOpt, line) ->
            Exists(t, var, mapTypeCstr exprCond, mapTypeCstr expr, f tcOpt, line)
        | Tuple(expr1, expr2, tcOpt, line) -> Tuple(mapTypeCstr expr1, mapTypeCstr expr2, f tcOpt, line)
        | TupleFst(exprTuple, tcOpt, line) -> TupleFst(mapTypeCstr exprTuple, f tcOpt, line)
        | TupleSnd(exprTuple, tcOpt, line) -> TupleSnd(mapTypeCstr exprTuple, f tcOpt, line)
        | ListCons(exprElem, exprList, tcOpt, line) ->
            ListCons(mapTypeCstr exprElem, mapTypeCstr exprList, f tcOpt, line)
        | ListNth(exprList, exprIdx, tcOpt, line) -> ListNth(mapTypeCstr exprList, mapTypeCstr exprIdx, f tcOpt, line)
        | SetRange(exprLower, experUppwer, tcOpt, line) ->
            SetRange(mapTypeCstr exprLower, mapTypeCstr experUppwer, f tcOpt, line)
        | SetInsert(exprElem, exprSet, tcOpt, line) ->
            SetInsert(mapTypeCstr exprElem, mapTypeCstr exprSet, f tcOpt, line)
        | SetMem(exprElem, exprSet, tcOpt, line) -> SetMem(mapTypeCstr exprElem, mapTypeCstr exprSet, f tcOpt, line)
        | MapAdd(exprKey, exprVal, exprMap, tcOpt, line) ->
            MapAdd(mapTypeCstr exprKey, mapTypeCstr exprVal, mapTypeCstr exprMap, f tcOpt, line)
        | MapFindOpt(exprKey, exprMap, tcOpt, line) ->
            MapFindOpt(mapTypeCstr exprKey, mapTypeCstr exprMap, f tcOpt, line)
        | Univ(t, tcOpt, line) -> Univ(t, f tcOpt, line)

    mapTypeCstr expr
