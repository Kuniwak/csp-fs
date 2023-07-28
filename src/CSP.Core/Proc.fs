module CSP.Core.Proc

open FSharpx.Collections
open CSP.Core.Search
open CSP.Core.LineNum
open CSP.Core.Var
open CSP.Core.Ctor
open CSP.Core.Expr

type ProcId = string

type Proc<'a> =
    | Unwind of ProcId * Expr<'a> option * LineNum
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

let format (fmt: string -> 'a -> string) (m: Map<ProcId, 'Var option * Proc<'a>>) (p0: Proc<'a>) : string =
    let format = format fmt in

    let rec f p isTop =
        match p with
        | Unwind(n, eOpt, _) ->
            if isTop then
                match (Map.find n m, eOpt) with
                | (Some var, p), Some e -> $"{f p false} ({var} = {format e})"
                | (None, p), None -> f p false
                | (None, _), Some _ -> "error: given a value to Unwind, but not needed"
                | (Some _, _), None -> "error: needed a value by Unwind, but not given"
            else
                match eOpt with
                | Some e -> $"({n} {format e})"
                | None -> $"{n}"
        | Stop _ -> "STOP"
        | Skip _ -> "SKIP"
        | Prefix(expr, p', _) -> $"({format expr} -> {f p' false})"
        | PrefixRecv(expr, var, p', _) -> $"({format expr}?{var} -> {f p' false})"
        | IntCh(p1, p2, _) -> $"({f p1 false} ⨅ {f p2 false})"
        | ExtCh(p1, p2, _) -> $"({f p1 false} □ {f p2 false})"
        | Seq(p1, p2, _) -> $"({f p1 false} ; {f p2 false})"
        | If(expr, p1, p2, _) -> $"(if {format expr} then {f p1 false} else {f p2 false})"
        | Match(expr, cs, _) ->
            let sep = " | " in

            let cs' =
                List.map (fun (ctorOpt, (varOpts, p')) -> $"{ctorOpt} {varOpts} -> {f p' false}") (Map.toList cs)

            $"(match {format expr} with {String.concat sep cs'})"
        | InterfaceParallel(p1, expr, p2, _) -> $"({f p1 false} ⟦{format expr}⟧ {f p2 false})"
        | Interleave(p1, p2, _) -> $"({f p1 false} ||| {f p2 false})"
        | Hide(p, expr, _) -> $"({f p false} \\\\ {format expr})"
        | Guard(e, p, _) -> $"({format e}&{f p false})"

    f p0 true

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

let map (f: Expr<'a> -> 'b) (p: Proc<'a>) : Proc<'b> =
    let rec map p =
        match p with
        | Unwind(pn, expr, line) -> Unwind(pn, Option.map (Expr.map f) expr, line)
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
