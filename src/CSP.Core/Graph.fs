module CSP.Core.Graph

open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.Val
open CSP.Core.Event
open CSP.Core.State
open CSP.Core.Trans
open CSP.Core.Search

let graph
    (max: int)
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (genv: Env<'Var, 'Ctor>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : (State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * int) list *
      (State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Event<'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) list =
    let s0: State<'P, 'Ev, 'Ch, 'Var, 'Ctor> = init m genv n vOpt in
    let mutable ss: (State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * int) list = [] in
    let mutable es: (State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Event<'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) list = [] in
    bfs
        (fun s es' ->
           ss <- (s, List.length es') :: ss
           es <- (List.map (fun (e, s') -> (s, e, s')) es') @ es)
        max
        (trans m genv)
        (unwind m)
        s0;
    (ss, es)
   

let dot
    (max: int)
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (genv: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : string =
    let dq = "\""
    let sq = "'"
    let ss, es = graph max m genv n vOpt

    let r1 =
        List.map
            (fun (s, n) ->
            match s with
            | Omega -> $"  \"{(format m s).Replace(dq, sq)}\""
            | _ when n = 0 -> $"  \"{(format m s).Replace(dq, sq)}\"  [fillcolor=red, style=filled, fontcolor=white]"
            | _ -> $"  \"{(format m s).Replace(dq, sq)}\"")
            ss
    let r2 =
        List.map
            (fun (s, ev, s') ->
                $"  \"{(format m s).Replace(dq, sq)}\" -> \"{(format m s').Replace(dq, sq)}\" [label=\"{(Event.format ev).Replace(dq, sq)}\"]")
            es

    let sep = "\n"

    $"""digraph G {{
{String.concat sep r1}
{String.concat sep r2}
}}"""
