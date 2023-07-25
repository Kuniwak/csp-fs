module CSP.Core.Expr

open CSP.Core.Indent
open CSP.Core.Ctor
open CSP.Core.LineNum
open CSP.Core.Search
open CSP.Core.Type
open CSP.Core.Var


type UserDefinedErrorMessage = string

type Expr<'a> =
    | LitTrue of 'a * LineNum
    | LitFalse of 'a * LineNum
    | LitNat of uint * 'a * LineNum
    | LitEmpty of Type * 'a * LineNum
    | VarRef of Var * 'a * LineNum
    | Union of Ctor * Expr<'a> list * 'a * LineNum
    | Tuple of Expr<'a> list * 'a * LineNum
    | If of Expr<'a> * Expr<'a> * Expr<'a> * 'a * LineNum
    | Match of Expr<'a> * Map<Ctor, Var list * Expr<'a>> * (Var option * Expr<'a>) option * 'a * LineNum
    | Eq of Type * Expr<'a> * Expr<'a> * 'a * LineNum
    | Less of Type * Expr<'a> * Expr<'a> * 'a * LineNum
    | Plus of Type * Expr<'a> * Expr<'a> * 'a * LineNum
    | Minus of Type * Expr<'a> * Expr<'a> * 'a * LineNum
    | Times of Type * Expr<'a> * Expr<'a> * 'a * LineNum
    | Size of Type * Expr<'a> * 'a * LineNum
    | Filter of Type * Var * Expr<'a> * Expr<'a> * 'a * LineNum
    | Exists of Type * Var * Expr<'a> * Expr<'a> * 'a * LineNum
    | TupleNth of Expr<'a> * uint * 'a * LineNum
    | ListNth of Expr<'a> * Expr<'a> * 'a * LineNum
    | BoolNot of Expr<'a> * 'a * LineNum
    | ListCons of Expr<'a> * Expr<'a> * 'a * LineNum
    | SetRange of Expr<'a> * Expr<'a> * 'a * LineNum
    | SetInsert of Expr<'a> * Expr<'a> * 'a * LineNum
    | SetMem of Expr<'a> * Expr<'a> * 'a * LineNum
    | MapAdd of Expr<'a> * Expr<'a> * Expr<'a> * 'a * LineNum
    | MapFindOpt of Expr<'a> * Expr<'a> * 'a * LineNum
    | Univ of Type * 'a * LineNum

let noAnnotation (s: string) (_: 'a) : string = s
let typeAnnotation (s: string) (t: Type) : string = $"(%s{s}::%s{Type.format t})"

let format (fmt: string -> 'a -> string) (expr: Expr<'a>) : string =
    let rec format indent expr =
        let indent1 = indent + 1u in
        let indent2 = indent + 2u in
        let indent3 = indent + 3u in

        match expr with
        | LitTrue(x, _) -> fmt "true" x
        | LitFalse(x, _) -> fmt "false" x
        | LitNat(n, x, _) -> fmt $"%d{n}" x
        | LitEmpty(t, x, _) -> fmt $"%s{Type.format t}.empty" x
        | Union(ctor, exprs, x, _) ->
            match List.length exprs with
            | 0 -> fmt (Ctor.format ctor) x
            | 1 -> fmt $"(%s{Ctor.format ctor} %s{format indent1 exprs[0]})" x
            | _ ->
                let s =
                    String.concat "\n" (List.map (fun expr -> $"{render indent}%s{format indent1 expr})") exprs)

                fmt $"(%s{Ctor.format ctor}\n%s{s})" x
        | If(e1, e2, e3, x, _) ->
            fmt
                $"""(if %s{format indent1 e1}
{render indent}then
{render indent1}%s{format indent2 e2}
{render indent}else
{render indent1}%s{format indent2 e3})"""
                x
        | Match(e, cs, d, x, _) ->
            let lines =
                String.concat
                    ""
                    (List.map
                        (fun (ctor, (vars, e')) ->
                            match List.length vars with
                            | 0 ->
                                $"\n{render indent}| %s{Ctor.format ctor} ->\n{render indent1}%s{format indent2 e'}"
                            | _ ->
                                let s = String.concat " " (List.map Var.format vars) in
                                $"\n{render indent}| %s{Ctor.format ctor} %s{s} ->\n{render indent1}%s{format indent2 e'}")
                        (Map.toList cs)) in

            match d with
            | Some(Some v, e') ->
                fmt
                    $"""(match %s{format indent1 e} with%s{lines}
{render indent}| %s{Var.format v} ->
{render indent1}%s{format indent2 e'})"""
                    x
            | Some(None, e') ->
                fmt
                    $"""(match %s{format indent1 e} with{lines}
{render indent}| _ ->
{render indent1}%s{format indent2 e'})"""
                    x
            | None -> fmt $"(match %s{format indent1 e} with%s{lines})" x
        | VarRef(v, x, _) -> fmt (Var.format v) x
        | BoolNot(expr, x, _) -> fmt $"""(not %s{format indent2 expr})""" x
        | Eq(t, expr1, expr2, x, _) ->
            fmt
                $"""(%s{Type.format t}.equal
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                x
        | Less(t, expr1, expr2, x, _) ->
            fmt
                $"""(%s{Type.format t}.less
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                x
        | Plus(t, expr1, expr2, x, _) ->
            fmt
                $"""(%s{Type.format t}.plus
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                x
        | Minus(t, expr1, expr2, x, _) ->
            fmt
                $"""(%s{Type.format t}.minus
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                x
        | Times(t, expr1, expr2, x, _) ->
            fmt
                $"""(%s{Type.format t}.times
{render indent1}%s{format indent2 expr1}
{render indent1}%s{format indent2 expr2})"""
                x
        | Size(t, expr, x, _) -> fmt $"""(%s{Type.format t}.count %s{format indent2 expr})""" x
        | Exists(t, var, expr1, expr2, x, _) ->
            fmt
                $"""(%s{Type.format t}.exists
{render indent1}(fun %s{Var.format var} ->
{render indent2}%s{format indent3 expr1})
{render indent1}%s{format indent2 expr2})"""
                x
        | Filter(t, var, expr1, expr2, x, _) ->
            fmt
                $"(%s{Type.format t}.filter
{render indent1}(fun %s{Var.format var} ->
{render indent2}%s{format indent3 expr1})
{render indent1}%s{format indent2 expr2})"
                x
        | Tuple(exprs, x, _) ->
            let s = String.concat $",\n%s{render indent1}" (List.map (format indent2) exprs)

            fmt $"(%s{s})" x
        | ListCons(e1, e2, x, _) ->
            fmt
                $"""(List.cons
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                x
        | TupleNth(e1, idx, x, _) ->
            fmt
                $"""(Tuple.nth
{render indent1}%s{format indent2 e1}
{render indent1}%d{idx})"""
                x
        | ListNth(e1, e2, x, _) ->
            fmt
                $"""(List.nth
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                x
        | SetRange(e1, e2, x, _) ->
            fmt
                $"""(Set.range
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                x
        | SetInsert(e1, e2, x, _) ->
            fmt
                $"""(Set.insert
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                x
        | SetMem(e1, e2, x, _) ->
            fmt
                $"""(Set.contains
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                x
        | MapAdd(k, v, m, x, _) ->
            fmt
                $"""(Map.add
{render indent1}%s{format indent2 k}
{render indent1}%s{format indent2 v}
{render indent1}%s{format indent2 m})"""
                x
        | MapFindOpt(e1, e2, x, _) ->
            fmt
                $"""(Map.findOpt
{render indent1}%s{format indent2 e1}
{render indent1}%s{format indent2 e2})"""
                x
        | Univ(t, x, _) -> fmt $"(univ::%s{Type.format t})" x

    format 0u expr

let line (expr: Expr<'a>) : LineNum =
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

let get (expr: Expr<'a>) : 'a =
    match expr with
    | LitTrue(x, _) -> x
    | LitFalse(x, _) -> x
    | LitNat(_, x, _) -> x
    | LitEmpty(_, x, _) -> x
    | Union(_, _, x, _) -> x
    | If(_, _, _, x, _) -> x
    | Match(_, _, _, x, _) -> x
    | VarRef(_, x, _) -> x
    | BoolNot(_, x, _) -> x
    | Plus(_, _, _, x, _) -> x
    | Times(_, _, _, x, _) -> x
    | Minus(_, _, _, x, _) -> x
    | Less(_, _, _, x, _) -> x
    | Eq(_, _, _, x, _) -> x
    | Size(_, _, x, _) -> x
    | Filter(_, _, _, _, x, _) -> x
    | Exists(_, _, _, _, x, _) -> x
    | Tuple(_, x, _) -> x
    | TupleNth(_, _, x, _) -> x
    | ListCons(_, _, x, _) -> x
    | ListNth(_, _, x, _) -> x
    | SetRange(_, _, x, _) -> x
    | SetInsert(_, _, x, _) -> x
    | SetMem(_, _, x, _) -> x
    | MapAdd(_, _, _, x, _) -> x
    | MapFindOpt(_, _, x, _) -> x
    | Univ(_, x, _) -> x

let children (expr: Expr<'a>) : Expr<'a> list =
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


let dfs (visit: Expr<'a> -> Unit) (expr: Expr<'a>) : Unit =
    dfs
        searchCfgUnlimited
        (fun expr _ -> visit expr)
        (fun expr -> List.map (fun expr -> ((), expr)) (children expr))
        id
        expr

let bfs (visit: Expr<'a> -> Unit) (expr: Expr<'a>) : Unit =
    bfs
        searchCfgUnlimited
        (fun expr _ -> visit expr)
        (fun expr -> List.map (fun expr -> ((), expr)) (children expr))
        id
        expr

let rec fold (f: 'State -> Expr<'a> -> 'State) (s: 'State) (expr: Expr<'a>) =
    let exprs = children expr in f (List.fold (fold f) s exprs) expr

let map (f: Expr<'a> -> 'b) (expr: Expr<'a>) : Expr<'b> =
    let rec mapType expr =
        let y = f expr in
        match expr with
        | LitTrue(_, line) -> LitTrue(y, line)
        | LitFalse(_, line) -> LitFalse(y, line)
        | LitNat(n, _, line) -> LitNat(n, y, line)
        | LitEmpty(t, _, line) -> LitEmpty(t, y, line)
        | Union(ctor, exprs, _, line) -> Union(ctor, List.map mapType exprs, y, line)
        | If(exprCond, exprThen, exprFalse, _, line) ->
            If(mapType exprCond, mapType exprThen, mapType exprFalse, y, line)
        | Match(exprUnion, exprMap, exprDef, _, line) ->
            Match(
                mapType exprUnion,
                Map.map (fun _ (var, expr) -> (var, mapType expr)) exprMap,
                Option.map (fun (var, expr) -> (var, mapType expr)) exprDef,
                y,
                line
            )
        | VarRef(var, _, line) -> VarRef(var, y, line)
        | BoolNot(expr, _, line) -> BoolNot(mapType expr, y, line)
        | Plus(t, expr1, expr2, _, line) -> Plus(t, mapType expr1, mapType expr2, y, line)
        | Times(t, expr1, expr2, _, line) -> Times(t, mapType expr1, mapType expr2, y, line)
        | Minus(t, expr1, expr2, _, line) -> Minus(t, mapType expr1, mapType expr2, y, line)
        | Less(t, expr1, expr2, _, line) -> Less(t, mapType expr1, mapType expr2, y, line)
        | Eq(t, expr1, expr2, _, line) -> Eq(t, mapType expr1, mapType expr2, y, line)
        | Size(t, expr, _, line) -> Size(t, mapType expr, y, line)
        | Filter(t, var, exprCond, expr, _, line) -> Filter(t, var, mapType exprCond, mapType expr, y, line)
        | Exists(t, var, exprCond, expr, _, line) -> Exists(t, var, mapType exprCond, mapType expr, y, line)
        | Tuple(exprs, _, line) -> Tuple(List.map mapType exprs, y, line)
        | ListCons(exprElem, exprList, _, line) -> ListCons(mapType exprElem, mapType exprList, y, line)
        | TupleNth(exprList, idx, _, line) -> TupleNth(mapType exprList, idx, y, line)
        | ListNth(exprList, exprIdx, _, line) -> ListNth(mapType exprList, mapType exprIdx, y, line)
        | SetRange(exprLower, exprUpper, _, line) -> SetRange(mapType exprLower, mapType exprUpper, y, line)
        | SetInsert(exprElem, exprSet, _, line) -> SetInsert(mapType exprElem, mapType exprSet, y, line)
        | SetMem(exprElem, exprSet, _, line) -> SetMem(mapType exprElem, mapType exprSet, y, line)
        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            MapAdd(mapType exprKey, mapType exprVal, mapType exprMap, y, line)
        | MapFindOpt(exprKey, exprMap, _, line) -> MapFindOpt(mapType exprKey, mapType exprMap, y, line)
        | Univ(t, _, line) -> Univ(t, y, line)

    mapType expr
