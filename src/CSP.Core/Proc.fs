module CSP.Core.Proc

open FSharpx.Collections
open CSP.Core.Search
open CSP.Core.LineNum
open CSP.Core.Var
open CSP.Core.Ctor
open CSP.Core.Expr

type ProcId = string

type Proc<'a> =
    | Unwind of ProcId * Expr<'a> list * LineNum
    | Stop of LineNum
    | Skip of LineNum
    | Prefix of Expr<'a> * Proc<'a> * LineNum
    | PrefixRecv of Expr<'a> * Var * Proc<'a> * LineNum
    | IntCh of Proc<'a> * Proc<'a> * LineNum
    | ExtCh of Proc<'a> * Proc<'a> * LineNum
    | Seq of Proc<'a> * Proc<'a> * LineNum
    | If of Expr<'a> * Proc<'a> * Proc<'a> * LineNum
    | Match of Expr<'a> * Map<Ctor option, Var option list * Proc<'a>> * LineNum
    | InterfaceParallel of Proc<'a> * Expr<'a> * Proc<'a> * LineNum
    | Interleave of Proc<'a> * Proc<'a> * LineNum
    | Hide of Proc<'a> * Expr<'a> * LineNum
    | Guard of Expr<'a> * Proc<'a> * LineNum

let get (p: Proc<'a>) : 'a list =
    match p with
    | Unwind(_, exprs, _) -> List.map get exprs
    | Stop _ -> []
    | Skip _ -> []
    | Prefix(expr, _, _) -> [ get expr ]
    | PrefixRecv(expr, _, _, _) -> [ get expr ]
    | IntCh _ -> []
    | ExtCh _ -> []
    | Seq _ -> []
    | If(expr, _, _, _) -> [ get expr ]
    | Match(expr, _, _) -> [ get expr ]
    | InterfaceParallel(_, expr, _, _) -> [ get expr ]
    | Interleave _ -> []
    | Hide(_, expr, _) -> [ get expr ]
    | Guard(expr, _, _) -> [ get expr ]

let format (annotation: string -> 'a -> string) (p0: Proc<'a>) : string =
    let formatExpr = format annotation in

    let rec format p =
        match p with
        | Unwind(n, exprs, _) ->
            let s = String.concat "" (List.map (fun expr -> $" %s{formatExpr expr}") exprs)
            $"(%s{n}%s{s})"
        | Stop _ -> "STOP"
        | Skip _ -> "SKIP"
        | Prefix(expr, p', _) -> $"({formatExpr expr} -> {format p'})"
        | PrefixRecv(expr, var, p', _) -> $"({formatExpr expr}?{var} -> {format p'})"
        | IntCh(p1, p2, _) -> $"({format p1} ⨅ {format p2})"
        | ExtCh(p1, p2, _) -> $"({format p1} □ {format p2})"
        | Seq(p1, p2, _) -> $"({format p1} ; {format p2})"
        | If(expr, p1, p2, _) -> $"(if {formatExpr expr} then {format p1} else {format p2})"
        | Match(expr, cs, _) ->
            let sep = " | " in

            let cs' =
                List.map (fun (ctorOpt, (varOpts, p')) -> $"{ctorOpt} {varOpts} -> {format p'}") (Map.toList cs)

            $"(match {formatExpr expr} with {String.concat sep cs'})"
        | InterfaceParallel(p1, expr, p2, _) -> $"({format p1} ⟦{formatExpr expr}⟧ {format p2})"
        | Interleave(p1, p2, _) -> $"({format p1} ||| {format p2})"
        | Hide(p, expr, _) -> $"({format p} \\\\ {formatExpr expr})"
        | Guard(e, p, _) -> $"({formatExpr e}&{format p})"

    format p0

let line (p: Proc<'a>) : LineNum =
    match p with
    | Unwind(_, _, line) -> line
    | Stop line -> line
    | Skip line -> line
    | Prefix(_, _, line) -> line
    | PrefixRecv(_, _, _, line) -> line
    | IntCh(_, _, line) -> line
    | ExtCh(_, _, line) -> line
    | Seq(_, _, line) -> line
    | If(_, _, _, line) -> line
    | Match(_, _, line) -> line
    | InterfaceParallel(_, _, _, line) -> line
    | Interleave(_, _, line) -> line
    | Hide(_, _, line) -> line
    | Guard(_, _, line) -> line

let children (p: Proc<'a>) : Proc<'a> list =
    match p with
    | Unwind _ -> []
    | Stop _ -> []
    | Skip _ -> []
    | Prefix(_, p, _) -> [ p ]
    | PrefixRecv(_, _, p, _) -> [ p ]
    | IntCh(p1, p2, _) -> [ p1; p2 ]
    | ExtCh(p1, p2, _) -> [ p1; p2 ]
    | Seq(p1, p2, _) -> [ p1; p2 ]
    | If(_, p1, p2, _) -> [ p1; p2 ]
    | Match(_, mp, _) -> List.ofSeq (Seq.map snd (Map.values mp))
    | InterfaceParallel(p1, _, p2, _) -> [ p1; p2 ]
    | Interleave(p1, p2, _) -> [ p1; p2 ]
    | Hide(p, _, _) -> [ p ]
    | Guard(_, p, _) -> [ p ]

let dfs (visit: Proc<'a> -> Unit) (p: Proc<'a>) : Unit =
    Search.dfs searchCfgUnlimited (fun p _ -> visit p) (fun p -> List.map (fun p -> ((), p)) (children p)) id p

let bfs (visit: Proc<'a> -> Unit) (p: Proc<'a>) : Unit =
    Search.bfs searchCfgUnlimited (fun p _ -> visit p) (fun p -> List.map (fun p -> ((), p)) (children p)) id p

let rec fold (f: 'State -> Proc<'a> -> 'State) (s: 'State) (p: Proc<'a>) =
    let ps = children p in f (List.fold (fold f) s ps) p

let rec error (p: Proc<Result<'a, 'b>>) : 'b option =
    fold
        (fun opt p ->
            match opt with
            | Some err -> Some err
            | None ->
                List.fold
                    (fun opt res ->
                        match opt with
                        | Some(e) -> Some(e)
                        | None ->
                            match res with
                            | Ok _ -> None
                            | Error(e) -> Some(e))
                    None
                    (get p))
        None
        p

let map (f: Expr<'a> -> 'b) (p: Proc<'a>) : Proc<'b> =
    let rec map p =
        match p with
        | Unwind(pn, exprs, line) -> Unwind(pn, List.map (Expr.map f) exprs, line)
        | Stop(line) -> Stop(line)
        | Skip(line) -> Skip(line)
        | Prefix(expr, p, line) -> Prefix(Expr.map f expr, map p, line)
        | PrefixRecv(expr, var, p, line) -> PrefixRecv(Expr.map f expr, var, map p, line)
        | IntCh(p1, p2, line) -> IntCh(map p1, map p2, line)
        | ExtCh(p1, p2, line) -> ExtCh(map p1, map p2, line)
        | Seq(p1, p2, line) -> Seq(map p1, map p2, line)
        | If(expr, p1, p2, line) -> If(Expr.map f expr, map p1, map p2, line)
        | Match(expr, pm, line) -> Match(Expr.map f expr, Map.map (fun _ (varOpt, p) -> (varOpt, map p)) pm, line)
        | InterfaceParallel(p1, expr, p2, line) -> InterfaceParallel(map p1, Expr.map f expr, map p2, line)
        | Interleave(p1, p2, line) -> Interleave(map p1, map p2, line)
        | Hide(p, expr, line) -> Hide(map p, Expr.map f expr, line)
        | Guard(expr, p, line) -> Guard(Expr.map f expr, map p, line)

    map p
