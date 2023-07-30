module CSP.Core.StateSpace

open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.Proc
open CSP.Core.ProcEval
open CSP.Core.ProcEvalError
open CSP.Core.ProcMap
open CSP.Core.State
open CSP.Core.Univ
open CSP.Core.Val
open CSP.Core.Util

type NamedConfig =
    { UnivConfig: UnivConfig
      ProcEvalConfig: ProcEvalConfig }

let namedSpace
    (cfg: NamedConfig)
    (cm: CtorMap)
    (pm: ProcMap<unit>)
    (genv: Env)
    : Result<Map<State, (ProcId * Val list) list>, ProcEvalError> =
    pm
    |> fold
        (fun mRes pn (xs, p) ->
            mRes
            |> Result.bind (fun m ->
                xs
                |> ResultEx.bindAll (fun (var, t) ->
                    univ cfg.UnivConfig t |> Result.map (List.map (fun v -> (var, v))))
                |> Result.mapError UnivError
                |> Result.map ListEx.cartesian
                |> Result.map (List.map (fun xs -> (bindAll xs genv, List.map snd xs)))
                |> Result.bind (
                    ResultEx.bindAll (fun (env, vs) ->
                        eval cfg.ProcEvalConfig cm env p |> Result.map (fun s -> (s, pn, vs)))
                )
                |> Result.map (fun xs ->
                    List.fold
                        (fun m (s, pn, vs) ->
                            match Map.tryFind s m with
                            | Some(xs) -> Map.add s ((pn, vs) :: xs) m
                            | None -> Map.add s [ (pn, vs) ] m)
                        m
                        xs)))
        (Ok(Map.empty))
