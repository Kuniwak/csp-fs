module CSP.Core.Proc

open FSharpx.Collections
open CSP.Core.Type
open CSP.Core.Expr

type Proc<'P, 'Var, 'Ctor when 'P: comparison and 'Var: comparison and 'Ctor: comparison> =
    | Unwind of 'P * Expr<'Var, 'Ctor> option
    | Stop
    | Skip
    | Prefix of Expr<'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor>
    | PrefixRecv of Expr<'Var, 'Ctor> * 'Var * Proc<'P, 'Var, 'Ctor>
    | IntCh of Proc<'P, 'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor>
    | ExtCh of Proc<'P, 'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor>
    | Seq of Proc<'P, 'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor>
    | If of Expr<'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor>
    | Match of
        Expr<'Var, 'Ctor> *
        Map<Ctor<'Ctor>, 'Var * Proc<'P, 'Var, 'Ctor>> *
        ('Var option * Proc<'P, 'Var, 'Ctor>) option
    | InterfaceParallel of Proc<'P, 'Var, 'Ctor> * Expr<'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor>
    | Interleave of Proc<'P, 'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor>
    | Hide of Proc<'P, 'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Guard of Expr<'Var, 'Ctor> * Proc<'P, 'Var, 'Ctor>

let format (m: Map<'P, 'Var option * Proc<'P, 'Var, 'Ctor>>) (p0: Proc<'P, 'Var, 'Ctor>) : string =
    let rec f p isTop =
        match p with
        | Unwind(n, eOpt) ->
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
        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(expr, p') -> $"({format expr} -> {f p' false})"
        | PrefixRecv(expr, var, p') -> $"({format expr}?{var} -> {f p' false})"
        | IntCh(p1, p2) -> $"({f p1 false} ⨅ {f p2 false})"
        | ExtCh(p1, p2) -> $"({f p1 false} □ {f p2 false})"
        | Seq(p1, p2) -> $"({f p1 false} ; {f p2 false})"
        | If(expr, p1, p2) -> $"(if {format expr} then {f p1 false} else {f p2 false})"
        | Match(expr, cs, dc) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, p')) -> $"{c} {v} -> {f p' false}") (Map.toList cs)

            match dc with
            | Some(v, p') -> $"(match {format expr} with {String.concat sep cs'} | {v} -> {f p' false})"
            | None -> $"(match {format expr} with {String.concat sep cs'})"
        | InterfaceParallel(p1, expr, p2) -> $"({f p1 false} ⟦{format expr}⟧ {f p2 false})"
        | Interleave(p1, p2) -> $"({f p1 false} ||| {f p2 false})"
        | Hide(p, expr) -> $"({f p false} \\\\ {format expr})"
        | Guard(e, p) -> $"({format e}&{f p false})"

    f p0 true
