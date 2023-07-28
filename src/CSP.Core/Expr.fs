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
    | Match of Expr<'a> * Map<Ctor option, Var option list * Expr<'a>> * 'a * LineNum
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
        | Match(e, cs, x, _) ->
            let s =
                String.concat
                    ""
                    (List.map
                        (fun (ctorOpt, (varOpts, e')) ->
                            match ctorOpt, List.length varOpts with
                            | Some ctor, 0 ->
                                $"""
{render indent}| %s{Ctor.format ctor} ->
{render indent1}%s{format indent2 e'}"""
                            | Some ctor, _ ->
                                let s =
                                    String.concat
                                        " "
                                        (List.map
                                            (fun varOpt ->
                                                match varOpt with
                                                | Some var -> Var.format var
                                                | None -> "_")
                                            varOpts) in

                                $"""
{render indent}| %s{Ctor.format ctor} %s{s} ->
{render indent1}%s{format indent2 e'}"""
                            | None, 0 ->
                                $"""
{render indent}| _ ->
{render indent1}%s{format indent2 e'}"""
                            | None, _ ->
                                let s =
                                    String.concat
                                        " "
                                        (List.map
                                            (fun varOpt ->
                                                match varOpt with
                                                | Some var -> Var.format var
                                                | None -> "_")
                                            varOpts) in

                                $"""
{render indent}| %s{s} ->
{render indent1}%s{format indent2 e'}""")
                        (Map.toList cs)) in

            fmt $"(match %s{format indent1 e} with %s{s})" x
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
    | Match(_, _, _, line) -> line
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
    | Match(_, _, x, _) -> x
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
    | Match(expr, vs, _, _) -> Map.fold (fun acc _ (_, expr) -> acc @ [ expr ]) [ expr ] vs
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

let rec error (expr: Expr<Result<'a, 'b>>) : 'b option =
    fold
        (fun opt expr ->
            match opt with
            | Some err -> Some err
            | None ->
                match get expr with
                | Ok _ -> None
                | Error e -> Some e)
        None
        expr

let map (f: Expr<'a> -> 'b) (expr: Expr<'a>) : Expr<'b> =
    let rec mapType expr =
        match expr with
        | LitTrue(_, line) -> LitTrue(f expr, line)
        | LitFalse(_, line) -> LitFalse(f expr, line)
        | LitNat(n, _, line) -> LitNat(n, f expr, line)
        | LitEmpty(t, _, line) -> LitEmpty(t, f expr, line)
        | Union(ctor, exprs, _, line) -> Union(ctor, List.map mapType exprs, f expr, line)
        | If(exprCond, exprThen, exprFalse, _, line) ->
            If(mapType exprCond, mapType exprThen, mapType exprFalse, f expr, line)
        | Match(exprUnion, exprMap, _, line) ->
            Match(mapType exprUnion, Map.map (fun _ (var, expr) -> (var, mapType expr)) exprMap, f expr, line)
        | VarRef(var, _, line) -> VarRef(var, f expr, line)
        | BoolNot(exprBool, _, line) -> BoolNot(mapType exprBool, f expr, line)
        | Plus(t, expr1, expr2, _, line) -> Plus(t, mapType expr1, mapType expr2, f expr, line)
        | Times(t, expr1, expr2, _, line) -> Times(t, mapType expr1, mapType expr2, f expr, line)
        | Minus(t, expr1, expr2, _, line) -> Minus(t, mapType expr1, mapType expr2, f expr, line)
        | Less(t, expr1, expr2, _, line) -> Less(t, mapType expr1, mapType expr2, f expr, line)
        | Eq(t, expr1, expr2, _, line) -> Eq(t, mapType expr1, mapType expr2, f expr, line)
        | Size(t, exprSize, _, line) -> Size(t, mapType exprSize, f expr, line)
        | Filter(t, var, exprCond, exprEnum, _, line) ->
            Filter(t, var, mapType exprCond, mapType exprEnum, f expr, line)
        | Exists(t, var, exprCond, exprEnum, _, line) ->
            Exists(t, var, mapType exprCond, mapType exprEnum, f expr, line)
        | Tuple(exprs, _, line) -> Tuple(List.map mapType exprs, f expr, line)
        | ListCons(exprElem, exprList, _, line) -> ListCons(mapType exprElem, mapType exprList, f expr, line)
        | TupleNth(exprList, idx, _, line) -> TupleNth(mapType exprList, idx, f expr, line)
        | ListNth(exprList, exprIdx, _, line) -> ListNth(mapType exprList, mapType exprIdx, f expr, line)
        | SetRange(exprLower, exprUpper, _, line) -> SetRange(mapType exprLower, mapType exprUpper, f expr, line)
        | SetInsert(exprElem, exprSet, _, line) -> SetInsert(mapType exprElem, mapType exprSet, f expr, line)
        | SetMem(exprElem, exprSet, _, line) -> SetMem(mapType exprElem, mapType exprSet, f expr, line)
        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            MapAdd(mapType exprKey, mapType exprVal, mapType exprMap, f expr, line)
        | MapFindOpt(exprKey, exprMap, _, line) -> MapFindOpt(mapType exprKey, mapType exprMap, f expr, line)
        | Univ(t, _, line) -> Univ(t, f expr, line)

    mapType expr
