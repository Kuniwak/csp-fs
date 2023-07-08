module CSP.Core.Proc

open CSP.Core.Val
open CSP.Core.Expr
open CSP.Core.EventSpec
open FSharpx.Collections

type Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor when 'Ev: comparison and 'Var: comparison and 'Ctor: comparison and 'Ch: comparison>
    =
    | Unwind of 'P * Expr<'Var, 'Ctor> option
    | Stop
    | Skip
    | Prefix of 'Ev * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | PrefixSend of 'Ch * Expr<'Var, 'Ctor> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | PrefixRecv of 'Ch * 'Var * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | IntCh of Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | ExtCh of Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Seq of Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | If of Expr<'Var, 'Ctor> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Match of
        Expr<'Var, 'Ctor> *
        Map<Ctor<'Ctor>, 'Var * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>> *
        ('Var * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>) option
    | InterfaceParallel of Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Set<EventSpec<'Ev, 'Ch>> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Interleave of Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Hide of Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Set<EventSpec<'Ev, 'Ch>>
    | Guard of Expr<'Var, 'Ctor> * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>

let format (m: Map<'P, 'Var option * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>>) (p0: Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>) : string =
    let rec f p isTop =
        match p with
        | Unwind (n, eOpt) ->
            if isTop
            then
                match (Map.find n m, eOpt) with
                | (Some var, p), Some e -> $"{f p false} ({var} = {Expr.format e})"
                | (None, p), None -> f p false
                | (None, _), Some _ -> "error: given a value to Unwind, but not needed"
                | (Some _, _), None -> "error: needed a value by Unwind, but not given"
            else
                match eOpt with
                | Some e -> $"({n} {Expr.format e})"
                | None -> $"{n}"
        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(ev, p') -> $"({ev} -> {f p' false})"
        | PrefixSend(ev, v, p') -> $"({ev}!{v} -> {f p' false})"
        | PrefixRecv(ev, v, p') -> $"({ev}?{v} -> {f p' false})"
        | IntCh(p1, p2) -> $"({f p1 false} ⨅ {f p2 false})"
        | ExtCh(p1, p2) -> $"({f p1 false} □ {f p2 false})"
        | Seq(p1, p2) -> $"({f p1 false} ; {f p2 false})"
        | If(e, p1, p2) -> $"(if {Expr.format e} then {f p1 false} else {f p2 false})"
        | Match(e, cs, d) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, p')) -> $"{c} {v} -> {f p' false}") (Map.toList cs)

            match d with
            | Some(v, p') -> $"(match {Expr.format e} with {String.concat sep cs'} | {v} -> {f p' false})"
            | None -> $"(match {Expr.format e} with {String.concat sep cs'})"
        | InterfaceParallel(p1, evs, p2) ->
            let sep = ", "
            let es' = List.map (fun ev -> $"{ev}") (Set.toList evs) in
            $"({f p1 false} ⟦{{{String.concat sep es'}}}⟧ {f p2 false})"
        | Interleave(p1, p2) -> $"({f p1 false} ||| {f p2 false})"
        | Hide(p, evs) -> 
            let evs' = List.map (fun ev -> $"{ev}") (Set.toList evs) in
            let sep = ", "
            $"({f p false} \\\\ {String.concat sep evs'})"
        | Guard(e, p) -> $"({Expr.format e}&{f p false})"
    f p0 true
