module CSP.Core.Graph

open FSharpPlus
open CSP.Core.Indent
open CSP.Core.Var
open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.Event
open CSP.Core.Val
open CSP.Core.State
open CSP.Core.Trans
open CSP.Core.Search

let graph
    (max: int)
    (pm: ProcMap)
    (cm: CtorMap)
    (genv: Env)
    (n: ProcId)
    (vOpt: Val option)
    : (State * int) list * (State * Event * State) list =
    let s0: State = init pm genv n vOpt in
    let mutable ss: (State * int) list = [] in

    let mutable es: (State * Event * State) list =
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
    (pm: ProcMap)
    (cm: CtorMap)
    (genv: Map<Var, Val>)
    (n: ProcId)
    (vOpt: Val option)
    : string =
    let dq = "\""
    let sq = "'"
    let ss, es = graph max pm cm genv n vOpt

    let r1 =
        List.map
            (fun (s, n) ->
                match s with
                | Omega -> $"  \"{String.replace dq sq (oneline (format pm cm genv s))}\""
                | _ when n = 0 ->
                    $"  \"{String.replace dq sq (oneline (format pm cm genv s))}\"  [fillcolor=red, style=filled, fontcolor=white]"
                | _ -> $"  \"{String.replace dq sq (oneline (format pm cm genv s))}\"")
            ss

    let r2 =
        List.map
            (fun (s, ev, s') ->
                $"  \"{String.replace dq sq (oneline(format pm cm genv s))}\" -> \"{String.replace dq sq (oneline (format pm cm genv s'))}\" [label=\"{String.replace dq sq (oneline (Event.format ev))}\"]")
            es

    let sep = "\n"

    $"""digraph G {{
{String.concat sep r1}
{String.concat sep r2}
}}"""
