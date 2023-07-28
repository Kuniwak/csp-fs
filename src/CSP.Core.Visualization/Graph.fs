module CSP.Core.Graph

open CSP.Core.Expr
open FSharpPlus
open CSP.Core.Indent
open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.Event
open CSP.Core.State
open CSP.Core.Trans
open CSP.Core.Search

type GraphConfig =
    { TransConfig: TransConfig
      SearchConfig: SearchConfig
      UnwindConfig: UnwindConfig
      InitConfig: InitConfig }

let graphConfig searchCfg evalCfg =
    { TransConfig = transConfig evalCfg
      SearchConfig = searchCfg
      UnwindConfig = unwindConfig evalCfg
      InitConfig = initConfig evalCfg }

let graph
    (cfg: GraphConfig)
    (pm: ProcMap<unit>)
    (cm: CtorMap)
    (genv: Env)
    (n: ProcId)
    (exprs: Expr<unit> list)
    : (State * int) list * (State * Event * State) list =
    let mutable ss: (State * int) list = [] in
    let mutable es: (State * Event * State) list = [] in

    match init cfg.InitConfig cm pm genv n exprs with
    | Error err -> ss <- [ (ErrorState(TransError.format err), 0) ]
    | Ok s0 ->
        bfs
            cfg.SearchConfig
            (fun s es' ->
                ss <- (s, List.length es') :: ss
                es <- (List.map (fun (e, s') -> (s, e, s')) es') @ es)
            (trans cfg.TransConfig pm cm genv)
            (fun s ->
                match unwind cfg.UnwindConfig pm cm genv s with
                | Error(err) -> ErrorState(UnwindError.format err)
                | Ok(s) -> s)
            s0

    (ss, es)


type DotConfig =
    { UnwindConfig: UnwindConfig
      GraphConfig: GraphConfig }

let dotConfig n cfg =
    { UnwindConfig = unwindConfig cfg
      GraphConfig = graphConfig n cfg }

let dot (cfg: DotConfig) (pm: ProcMap<unit>) (cm: CtorMap) (genv: Env) (n: ProcId) (exprs: Expr<unit> list) : string =
    let escape = String.replace "\"" "'" in

    let format s =
        match unwind cfg.UnwindConfig pm cm genv s with
        | Error(err) -> format genv (ErrorState(UnwindError.format err))
        | Ok(s) -> format genv s in

    let ss, es = graph cfg.GraphConfig pm cm genv n exprs in

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
