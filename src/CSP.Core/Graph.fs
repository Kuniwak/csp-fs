module CSP.Core.Graph

open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.Val
open CSP.Core.State
open CSP.Core.Trans
open CSP.Core.Search

let graph
    (max: int)
    (pm: ProcMap<'P, 'Var, 'Ctor>)
    (cm: CtorMap<'Ctor>)
    (genv: Env<'Var, 'Ctor>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : (State<'P, 'Var, 'Ctor> * int) list * (State<'P, 'Var, 'Ctor> * Event<'Ctor> * State<'P, 'Var, 'Ctor>) list =
    let s0: State<'P, 'Var, 'Ctor> = init pm genv n vOpt in
    let mutable ss: (State<'P, 'Var, 'Ctor> * int) list = [] in

    let mutable es: (State<'P, 'Var, 'Ctor> * Event<'Ctor> * State<'P, 'Var, 'Ctor>) list =
        [] in

    bfs
        (fun s es' ->
            ss <- (s, List.length es') :: ss
            es <- (List.map (fun (e, s') -> (s, e, s')) es') @ es)
        max
        (trans pm cm genv)
        (unwind pm cm)
        s0

    (ss, es)


let dot
    (max: int)
    (pm: ProcMap<'P, 'Var, 'Ctor>)
    (cm: CtorMap<'Ctor>)
    (genv: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : string =
    let dq = "\""
    let sq = "'"
    let ss, es = graph max pm cm genv n vOpt

    let r1 =
        List.map
            (fun (s, n) ->
                match s with
                | Omega -> $"  \"{(format pm cm s).Replace(dq, sq)}\""
                | _ when n = 0 ->
                    $"  \"{(format pm cm s).Replace(dq, sq)}\"  [fillcolor=red, style=filled, fontcolor=white]"
                | _ -> $"  \"{(format pm cm s).Replace(dq, sq)}\"")
            ss

    let r2 =
        List.map
            (fun (s, ev, s') ->
                $"  \"{(format pm cm s).Replace(dq, sq)}\" -> \"{(format pm cm s').Replace(dq, sq)}\" [label=\"{(formatEvent ev).Replace(dq, sq)}\"]")
            es

    let sep = "\n"

    $"""digraph G {{
{String.concat sep r1}
{String.concat sep r2}
}}"""
