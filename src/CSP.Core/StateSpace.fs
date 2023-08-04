module CSP.Core.StateSpace

open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.Event
open CSP.Core.Proc
open CSP.Core.ProcEval
open CSP.Core.ProcEvalError
open CSP.Core.ProcMap
open CSP.Core.State
open CSP.Core.Trans
open CSP.Core.UnionMap
open CSP.Core.Univ
open CSP.Core.Val
open CSP.Core.Util

type NamedSpace = Map<State, (ProcId * Val list) list>

type NamedSpaceConfig =
    { UnivConfig: UnivConfig
      ProcEvalConfig: ProcEvalConfig }

let namedSpace
    (cfg: NamedSpaceConfig)
    (um: UnionMap)
    (cm: CtorMap)
    (pm: ProcMap<unit>)
    (genv: Env)
    : Result<NamedSpace, ProcEvalError> =
    let univ = univ cfg.UnivConfig um in
    let eval = eval cfg.ProcEvalConfig um cm in

    pm
    |> ProcMap.fold
        (fun mRes pn (xs, p) ->
            mRes
            |> Result.bind (fun m ->
                xs
                |> ResultEx.bindAll (fun (var, t) -> univ t |> Result.map (List.map (fun v -> (var, v))))
                |> Result.mapError UnivError
                |> Result.map ListEx.cartesian
                |> Result.map (List.map (fun xs -> (bindAll xs genv, List.map snd xs)))
                |> Result.bind (ResultEx.bindAll (fun (env, vs) -> eval env p |> Result.map (fun s -> (s, pn, vs))))
                |> Result.map (
                    List.fold
                        (fun m (s, pn, vs) ->
                            match Map.tryFind s m with
                            | Some(xs) -> Map.add s ((pn, vs) :: xs) m
                            | None -> Map.add s [ (pn, vs) ] m)
                        m
                )))
        (Ok(Map.empty))

let normedTrans
    (cfg: TransConfig)
    (pm: ProcMap<unit>)
    (cm: CtorMap)
    (um: UnionMap)
    (genv: Env)
    (ns: Map<State, (ProcId * Val list) list>)
    (s: State)
    : Result<(Event * State) list, ProcEvalError> =
    trans cfg pm um cm genv s
    |> Result.map (
        List.map (fun (ev, s) ->
            match Map.tryFind s ns with
            | Some(pvs) -> let pn, vs = List.head pvs in (ev, Unwind(pn, vs))
            | None -> (ev, s))
    )
