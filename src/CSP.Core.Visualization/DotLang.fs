module CSP.Core.Visualization.DotLang

open FSharpPlus
open CSP.Core
open CSP.Core.Val
open CSP.Core.Indent
open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.Event
open CSP.Core.State
open CSP.Core.Trans
open CSP.Core.Search
open CSP.Core.ProcEval

type GraphConfig =
    { TransConfig: TransConfig
      ProcEvalConfig: ProcEvalConfig
      SearchConfig: SearchConfig }

let graph
    (cfg: GraphConfig)
    (pm: ProcMap<unit>)
    (cm: CtorMap)
    (genv: Env)
    (pn: ProcId)
    (vs: Val list)
    : (State * int) list * (State * Event * State) list =
    match init pm genv pn vs with
    | Error(err) -> ([ ErrorState(ProcMapError.format err), 0 ], [])
    | Ok(env, p) ->
        match eval cfg.ProcEvalConfig pm cm genv env p with
        | Error(err) -> ([ (ErrorState(ProcEvalError.format err), 0) ], [])
        | Ok s0 ->
            let mutable ss: (State * int) list = [] in
            let mutable es: (State * Event * State) list = [] in

            bfs
                cfg.SearchConfig
                (fun s es' ->
                    ss <- (s, List.length es') :: ss
                    es <- (List.map (fun (e, s') -> (s, e, s')) es') @ es)
                (fun s ->
                    match trans cfg.TransConfig pm cm genv s with
                    | Error(err) -> [ (ErrorEvent, ErrorState(ProcEvalError.format err)) ]
                    | Ok(ts) -> ts)
                id
                s0

            (ss, es)

type DotConfig = { GraphConfig: GraphConfig }

let dot (cfg: DotConfig) (pm: ProcMap<unit>) (cm: CtorMap) (genv: Env) (pn: ProcId) (vs: Val list) : string =
    let escape = String.replace "\"" "'" in
    let format = format genv in
    let ss, es = graph cfg.GraphConfig pm cm genv pn vs in

    let r1 =
        List.map
            (fun (s, n) ->
                match s with
                | Omega -> $"  \"%s{escape (oneline (format s))}\""
                | _ when n = 0 ->
                    $"  \"%s{escape (oneline (format s))}\"  [fillcolor=red, style=filled, fontcolor=white]"
                | _ -> $"  \"%s{escape (oneline (format s))}\"")
            ss

    let r2 =
        List.map
            (fun (s, ev, s') ->
                $"  \"%s{escape (oneline (format s))}\" -> \"%s{escape (oneline (format s'))}\" [label=\"%s{escape (oneline (Event.format ev))}\"]")
            es

    let sep = "\n"

    $"""digraph G {{
%s{String.concat sep r1}
%s{String.concat sep r2}
}}"""
