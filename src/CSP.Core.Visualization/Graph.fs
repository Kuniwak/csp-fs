module CSP.Core.Graph

open FSharpPlus
open CSP.Core.Indent
open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.Event
open CSP.Core.Val
open CSP.Core.State
open CSP.Core.Trans
open CSP.Core.Search

type GraphConfig =
    { TransConfig: TransConfig
      SearchConfig: SearchConfig
      UnwindConfig: UnwindConfig }

let graphConfig searchCfg evalCfg =
    { TransConfig = transConfig evalCfg
      SearchConfig = searchCfg
      UnwindConfig = unwindConfig evalCfg }

let graph
    (cfg: GraphConfig)
    (pm: ProcMap)
    (cm: CtorMap)
    (genv: Env)
    (n: ProcId)
    (vOpt: Val option)
    : (State * int) list * (State * Event * State) list =
    let s0: State = init pm genv n vOpt in
    let mutable ss: (State * int) list = [] in

    let mutable es: (State * Event * State) list = [] in

    bfs
        cfg.SearchConfig
        (fun s es' ->
            ss <- (s, List.length es') :: ss
            es <- (List.map (fun (e, s') -> (s, e, s')) es') @ es)
        (trans cfg.TransConfig pm cm genv)
        (unwind cfg.UnwindConfig pm cm genv)
        s0

    (ss, es)


type DotConfig =
    { UnwindConfig: UnwindConfig
      GraphConfig: GraphConfig }

let dotConfig n cfg =
    { UnwindConfig = unwindConfig cfg
      GraphConfig = graphConfig n cfg }

let dot (cfg: DotConfig) (pm: ProcMap) (cm: CtorMap) (genv: Env) (n: ProcId) (vOpt: Val option) : string =
    let escape = String.replace "\"" "'"
    let format = format cfg.UnwindConfig pm cm genv
    let ss, es = graph cfg.GraphConfig pm cm genv n vOpt

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
