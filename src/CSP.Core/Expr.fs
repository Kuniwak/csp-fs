module CSP.Core.Expr

open CSP.Core.Indent
open CSP.Core.Ctor
open CSP.Core.LineNum
open CSP.Core.Search
open CSP.Core.Type
open CSP.Core.Var


type UserDefinedErrorMessage = string

type Expr =
    | LitTrue of Type option * LineNum
    | LitFalse of Type option * LineNum
    | LitNat of uint * Type option * LineNum
    | LitEmpty of Type * Type option * LineNum
    | VarRef of Var * Type option * LineNum
    | Union of Ctor * Expr list * Type option * LineNum
    | Tuple of Expr list * Type option * LineNum
    | If of Expr * Expr * Expr * Type option * LineNum
    | Match of Expr * Map<Ctor, Var list * Expr> * (Var option * Expr) option * Type option * LineNum
    | Eq of Type * Expr * Expr * Type option * LineNum
    | Less of Type * Expr * Expr * Type option * LineNum
    | Plus of Type * Expr * Expr * Type option * LineNum
    | Minus of Type * Expr * Expr * Type option * LineNum
    | Times of Type * Expr * Expr * Type option * LineNum
    | Size of Type * Expr * Type option * LineNum
    | Filter of Type * Var * Expr * Expr * Type option * LineNum
    | Exists of Type * Var * Expr * Expr * Type option * LineNum
    | TupleNth of Expr * uint * Type option * LineNum
    | ListNth of Expr * Expr * Type option * LineNum
    | BoolNot of Expr * Type option * LineNum
    | ListCons of Expr * Expr * Type option * LineNum
    | SetRange of Expr * Expr * Type option * LineNum
    | SetInsert of Expr * Expr * Type option * LineNum
    | SetMem of Expr * Expr * Type option * LineNum
    | MapAdd of Expr * Expr * Expr * Type option * LineNum
    | MapFindOpt of Expr * Expr * Type option * LineNum
    | Univ of Type * Type option * LineNum

let typeAnn (s: string) (tcOpt: Type option) =
    match tcOpt with
    | Some tc -> $"({s}::{Type.format tc})"
    | None -> s

let format (expr: Expr) : string =
    let rec format indent expr =
        let indent1 = indent + 1u in
        let indent2 = indent + 2u in
        let indent3 = indent + 3u in

        match expr with
        | LitTrue(tcOpt, _) -> typeAnn "true" tcOpt
        | LitFalse(tcOpt, _) -> typeAnn "false" tcOpt
        | LitNat(n, tcOpt, _) -> typeAnn $"%d{n}" tcOpt
        | LitEmpty(t, tcOpt, _) -> typeAnn $"%s{Type.format t}.empty" tcOpt
        | Union(ctor, exprs, tcOpt, _) ->
            match List.length exprs with
            | 0 -> typeAnn $"%s{Ctor.format ctor}" tcOpt
            | 1 -> typeAnn $"(%s{Ctor.format ctor} %s{format indent1 exprs[0]})" tcOpt
            | _ ->
                let s =
                    String.concat "\n" (List.map (fun expr -> $"{render indent} %s{format indent1 expr})") exprs)

                typeAnn $"(%s{Ctor.format ctor}\n%s{s})" tcOpt
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
                        (fun (ctor, (vars, e')) ->
                            match List.length vars with
                            | 0 ->
                                $"\n{render indent}| %s{Ctor.format ctor} _ ->\n{render indent1}%s{format indent2 e'}"
                            | _ ->
                                let s = String.concat " " (List.map Var.format vars) in
                                $"\n{render indent}| %s{Ctor.format ctor} %s{s} ->\n{render indent1}%s{format indent2 e'}")
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
        | BoolNot(expr, tcOpt, _) -> typeAnn $"""(not %s{format indent2 expr})""" tcOpt
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
        | Tuple(exprs, tcOpt, _) ->
            let s =
                String.concat ",\n" (List.map (fun expr -> $"{render indent1}%s{format indent2 expr}") exprs)

            typeAnn $"(%s{s})" tcOpt
        | ListCons(e1, e2, tcOpt, _) ->
            typeAnn
                $"""(List.cons
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                tcOpt
        | TupleNth(e1, idx, tcOpt, _) ->
            typeAnn
                $"""(Tuple.nth
{render indent1}%s{format indent2 e1}
{render indent1}%d{idx})"""
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
    | LitTrue(_, line) -> line
    | LitFalse(_, line) -> line
    | LitNat(_, _, line) -> line
    | LitEmpty(_, _, line) -> line
    | Union(_, _, _, line) -> line
    | If(_, _, _, _, line) -> line
    | Match(_, _, _, _, line) -> line
    | VarRef(_, _, line) -> line
    | BoolNot(_, _, line) -> line
    | Plus(_, _, _, _, line) -> line
    | Times(_, _, _, _, line) -> line
    | Minus(_, _, _, _, line) -> line
    | Less(_, _, _, _, line) -> line
    | Eq(_, _, _, _, line) -> line
    | Size(_, _, _, line) -> line
    | Filter(_, _, _, _, _, line) -> line
    | Exists(_, _, _, _, _, line) -> line
    | Tuple(_, _, line) -> line
    | ListCons(_, _, _, line) -> line
    | TupleNth(_, _, _, line) -> line
    | ListNth(_, _, _, line) -> line
    | SetRange(_, _, _, line) -> line
    | SetInsert(_, _, _, line) -> line
    | SetMem(_, _, _, line) -> line
    | MapAdd(_, _, _, _, line) -> line
    | MapFindOpt(_, _, _, line) -> line
    | Univ(_, _, line) -> line

let toType (expr: Expr) : Type =
    match expr with
    | LitTrue(Some tc, _) -> tc
    | LitFalse(Some tc, _) -> tc
    | LitNat(_, Some tc, _) -> tc
    | LitEmpty(_, Some tc, _) -> tc
    | Union(_, _, Some tc, _) -> tc
    | If(_, _, _, Some tc, _) -> tc
    | Match(_, _, _, Some tc, _) -> tc
    | VarRef(_, Some tc, _) -> tc
    | BoolNot(_, Some tc, _) -> tc
    | Plus(_, _, _, Some tc, _) -> tc
    | Times(_, _, _, Some tc, _) -> tc
    | Minus(_, _, _, Some tc, _) -> tc
    | Less(_, _, _, Some tc, _) -> tc
    | Eq(_, _, _, Some tc, _) -> tc
    | Size(_, _, Some tc, _) -> tc
    | Filter(_, _, _, _, Some tc, _) -> tc
    | Exists(_, _, _, _, Some tc, _) -> tc
    | Tuple(_, Some tc, _) -> tc
    | TupleNth(_, _, Some tc, _) -> tc
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
    | LitTrue _ -> []
    | LitFalse _ -> []
    | LitNat _ -> []
    | LitEmpty _ -> []
    | Union(_, exprs, _, _) -> exprs
    | VarRef _ -> []
    | If(expr1, expr2, expr3, _, _) -> [ expr1; expr2; expr3 ]
    | Match(expr, vs, dOpt, _, _) ->
        let es = Map.fold (fun acc _ (_, expr) -> acc @ [ expr ]) [ expr ] vs in

        match dOpt with
        | Some(_, expr) -> es @ [ expr ]
        | None -> es
    | BoolNot(expr, _, _) -> [ expr ]
    | Eq(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Less(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Plus(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Minus(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Times(_, expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Tuple(exprs, _, _) -> exprs
    | Size(_, expr, _, _) -> [ expr ]
    | Filter(_, _, _, expr, _, _) -> [ expr ]
    | Exists(_, _, _, expr, _, _) -> [ expr ]
    | ListCons(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | TupleNth(expr, _, _, _) -> [ expr ]
    | ListNth(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | SetRange(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | SetInsert(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | SetMem(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | MapAdd(expr1, expr2, expr3, _, _) -> [ expr1; expr2; expr3 ]
    | MapFindOpt(expr1, expr2, _, _) -> [ expr1; expr2 ]
    | Univ _ -> []


let dfs (visit: Expr -> Unit) (expr: Expr) : Unit =
    dfs
        searchCfgUnlimited
        (fun expr _ -> visit expr)
        (fun expr -> List.map (fun expr -> ((), expr)) (children expr))
        id
        expr

let bfs (visit: Expr -> Unit) (expr: Expr) : Unit =
    bfs
        searchCfgUnlimited
        (fun expr _ -> visit expr)
        (fun expr -> List.map (fun expr -> ((), expr)) (children expr))
        id
        expr

let mapType (f: Type option -> Type option) (expr: Expr) =
    let rec mapType expr =
        match expr with
        | LitTrue(tcOpt, line) -> LitTrue(f tcOpt, line)
        | LitFalse(tcOpt, line) -> LitFalse(f tcOpt, line)
        | LitNat(n, tcOpt, line) -> LitNat(n, f tcOpt, line)
        | LitEmpty(t, tcOpt, line) -> LitEmpty(t, f tcOpt, line)
        | Union(ctor, exprs, tcOpt, line) -> Union(ctor, List.map mapType exprs, f tcOpt, line)
        | If(exprCond, exprThen, exprFalse, tcOpt, line) ->
            If(mapType exprCond, mapType exprThen, mapType exprFalse, f tcOpt, line)
        | Match(exprUnion, exprMap, exprDef, tcOpt, line) ->
            Match(
                mapType exprUnion,
                Map.map (fun _ (var, expr) -> (var, mapType expr)) exprMap,
                Option.map (fun (var, expr) -> (var, mapType expr)) exprDef,
                f tcOpt,
                line
            )
        | VarRef(var, tcOpt, line) -> VarRef(var, f tcOpt, line)
        | BoolNot(expr, tcOpt, line) -> BoolNot(mapType expr, f tcOpt, line)
        | Plus(t, expr1, expr2, tcOpt, line) -> Plus(t, mapType expr1, mapType expr2, f tcOpt, line)
        | Times(t, expr1, expr2, tcOpt, line) -> Times(t, mapType expr1, mapType expr2, f tcOpt, line)
        | Minus(t, expr1, expr2, tcOpt, line) -> Minus(t, mapType expr1, mapType expr2, f tcOpt, line)
        | Less(t, expr1, expr2, tcOpt, line) -> Less(t, mapType expr1, mapType expr2, f tcOpt, line)
        | Eq(t, expr1, expr2, tcOpt, line) -> Eq(t, mapType expr1, mapType expr2, f tcOpt, line)
        | Size(t, expr, tcOpt, line) -> Size(t, mapType expr, f tcOpt, line)
        | Filter(t, var, exprCond, expr, tcOpt, line) -> Filter(t, var, mapType exprCond, mapType expr, f tcOpt, line)
        | Exists(t, var, exprCond, expr, tcOpt, line) -> Exists(t, var, mapType exprCond, mapType expr, f tcOpt, line)
        | Tuple(exprs, tcOpt, line) -> Tuple(List.map mapType exprs, f tcOpt, line)
        | ListCons(exprElem, exprList, tcOpt, line) -> ListCons(mapType exprElem, mapType exprList, f tcOpt, line)
        | TupleNth(exprList, idx, tcOpt, line) -> TupleNth(mapType exprList, idx, f tcOpt, line)
        | ListNth(exprList, exprIdx, tcOpt, line) -> ListNth(mapType exprList, mapType exprIdx, f tcOpt, line)
        | SetRange(exprLower, exprUpper, tcOpt, line) -> SetRange(mapType exprLower, mapType exprUpper, f tcOpt, line)
        | SetInsert(exprElem, exprSet, tcOpt, line) -> SetInsert(mapType exprElem, mapType exprSet, f tcOpt, line)
        | SetMem(exprElem, exprSet, tcOpt, line) -> SetMem(mapType exprElem, mapType exprSet, f tcOpt, line)
        | MapAdd(exprKey, exprVal, exprMap, tcOpt, line) ->
            MapAdd(mapType exprKey, mapType exprVal, mapType exprMap, f tcOpt, line)
        | MapFindOpt(exprKey, exprMap, tcOpt, line) -> MapFindOpt(mapType exprKey, mapType exprMap, f tcOpt, line)
        | Univ(t, tcOpt, line) -> Univ(t, f tcOpt, line)

    mapType expr
