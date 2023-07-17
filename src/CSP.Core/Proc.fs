module CSP.Core.Proc

open CSP.Core.LineNum
open FSharpx.Collections
open CSP.Core.Ctor
open CSP.Core.Expr

type ProcId = string

type Proc =
    | Unwind of ProcId * Expr option * LineNum
    | Stop of LineNum
    | Skip of LineNum
    | Prefix of Expr * Proc * LineNum
    | PrefixRecv of Expr * string * Proc * LineNum
    | IntCh of Proc * Proc * LineNum
    | ExtCh of Proc * Proc * LineNum
    | Seq of Proc * Proc * LineNum
    | If of Expr * Proc * Proc * LineNum
    | Match of
        Expr *
        Map<Ctor, string * Proc> *
        (string option * Proc) option *
        LineNum
    | InterfaceParallel of Proc * Expr * Proc * LineNum
    | Interleave of Proc * Proc * LineNum
    | Hide of Proc * Expr * LineNum
    | Guard of Expr * Proc * LineNum

let format (m: Map<ProcId, 'Var option * Proc>) (p0: Proc) : string =
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
        | Match(expr, cs, dc, _) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, p')) -> $"{c} {v} -> {f p' false}") (Map.toList cs)

            match dc with
            | Some(v, p') -> $"(match {format expr} with {String.concat sep cs'} | {v} -> {f p' false})"
            | None -> $"(match {format expr} with {String.concat sep cs'})"
        | InterfaceParallel(p1, expr, p2, _) -> $"({f p1 false} ⟦{format expr}⟧ {f p2 false})"
        | Interleave(p1, p2, _) -> $"({f p1 false} ||| {f p2 false})"
        | Hide(p, expr, _) -> $"({f p false} \\\\ {format expr})"
        | Guard(e, p, _) -> $"({format e}&{f p false})"

    f p0 true
    
let line (p: Proc): LineNum =
    match p with
    | Unwind (_, _, line) -> line
    | Stop line -> line
    | Skip line -> line
    | Prefix (_, _, line) -> line
    | PrefixRecv (_, _, _, line) -> line
    | IntCh(_, _, line) -> line
    | ExtCh(_, _, line) -> line
    | Seq(_, _, line) -> line
    | If(_, _, _, line) -> line
    | Match(_, _, _, line) -> line
    | InterfaceParallel(_, _, _, line) -> line
    | Interleave(_, _, line) -> line
    | Hide(_, _, line) -> line
    | Guard(_, _, line) -> line

let children (p: Proc) : Proc list =
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
    | Match(_, mp, pOpt, _) ->
        let ps = List.map (fun (_, p) -> p) (Seq.toList (Map.values mp)) in

        match pOpt with
        | Some(_, p) -> ps @ [ p ]
        | None -> ps
    | InterfaceParallel(p1, _, p2, _) -> [ p1; p2 ]
    | Interleave(p1, p2, _) -> [ p1; p2 ]
    | Hide(p, _, _) -> [ p ]
    | Guard(_, p, _) -> [ p ]

let dfs (visit: Proc -> Unit) (p: Proc) : Unit =
    Search.dfs (fun p _ -> visit p) 10000 (fun p -> List.map (fun p -> ((), p)) (children p)) id p

let bfs (visit: Proc -> Unit) (p: Proc) : Unit =
    Search.bfs (fun p _ -> visit p) 10000 (fun p -> List.map (fun p -> ((), p)) (children p)) id p
