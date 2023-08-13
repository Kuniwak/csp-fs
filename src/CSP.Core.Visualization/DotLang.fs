module CSP.Core.Visualization.DotLang

open FSharpPlus
open CSP.Core
open CSP.Core.UnionMap
open CSP.Core.StateSpace
open CSP.Core.ProcEvalError
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
      SearchConfig: SearchConfig
      NamedConfig: NamedSpaceConfig }


let graph
    (cfg: GraphConfig)
    (pm: ProcMap<unit>)
    (um: UnionMap)
    (cm: CtorMap)
    (genv: Env)
    (p: Proc<unit>)
    : Result<(State * int) list * (State * Event * State) list, ProcEvalError> =
    eval cfg.ProcEvalConfig um cm genv p
    |> Result.bind (fun s ->
        let mutable ss: (State * int) list = [] in
        let mutable es: (State * Event * State) list = [] in

        namedSpace cfg.NamedConfig um cm pm genv
        |> Result.map (fun ns ->
            bfs
                cfg.SearchConfig
                (fun s es' ->
                    ss <- (s, List.length es') :: ss
                    es <- (List.map (fun (e, s') -> (s, e, s')) es') @ es)
                (fun s ->
                    match normedTrans cfg.TransConfig pm cm um genv ns s with
                    | Error(err) -> [ (ErrorEvent, ErrorState(ProcEvalError.format err)) ]
                    | Ok(ts) -> ts)
                s

            (ss, es)))


let dot
    (cfg: GraphConfig)
    (pm: ProcMap<unit>)
    (um: UnionMap)
    (cm: CtorMap)
    (genv: Env)
    (p: Proc<unit>)
    : Result<string, ProcEvalError> =
    let escape = String.replace "\"" "'" in
    let format = format genv in

    graph cfg pm um cm genv p
    |> Result.map (fun (ss, es) ->
        let r1 =
            ss
            |> List.map (fun (s, n) ->
                match s with
                | Omega -> $"  \"%s{escape (oneline (format s))}\""
                | _ when n = 0 ->
                    $"  \"%s{escape (oneline (format s))}\"  [fillcolor=red, style=filled, fontcolor=white]"
                | _ -> $"  \"%s{escape (oneline (format s))}\"")
            |> String.concat "\n"

        let r2 =
            es
            |> List.map (fun (s, ev, s') ->
                $"  \"%s{escape (oneline (format s))}\" -> \"%s{escape (oneline (format s'))}\" [label=\"%s{escape (oneline (Event.format ev))}\"]")
            |> String.concat "\n"

        $"""digraph G {{
%s{r1}

%s{r2}
}}""")
