module CSP.Core.Trans

open CSP.Core.Env
open CSP.Core.Type
open CSP.Core.Val
open CSP.Core.Expr
open CSP.Core.LineNum
open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.Event
open CSP.Core.TransError
open CSP.Core.State
open CSP.Core.Eval
open CSP.Core.UnwindError

type InitConfig = { EvalConfig: EvalConfig }

let initConfig (evalCfg: EvalConfig) : InitConfig = { EvalConfig = evalCfg }

let init
    (cfg: InitConfig)
    (cm: CtorMap)
    (pm: ProcMap<unit>)
    (genv: Env)
    (pn: ProcId)
    (exprs: Expr<unit> list)
    : Result<State, TransError> =
    match tryFind pn pm with
    | None -> failwith $"no such process: {pn}"
    | Some(varOpts, _) ->
        if List.length varOpts = List.length exprs then
            let vsRes =
                List.foldBack
                    (fun expr ->
                        Result.bind (fun vs -> Result.map (fun v -> v :: vs) (eval cfg.EvalConfig cm genv expr)))
                    exprs
                    (Ok([]))

            Result.map
                (fun env -> ofProc env (Proc.Unwind(pn, exprs, unknown)))
                (Result.bind
                    (fun vs -> Result.mapError TransError.EnvError (Env.bindAll (List.zip varOpts vs) genv))
                    (Result.mapError TransError.EvalError vsRes))
        else
            Error(UnwindError(ArgumentsLengthMismatch(List.length exprs, varOpts)))

type TransConfig =
    { EvalConfig: EvalConfig
      UnwindConfig: UnwindConfig }

let transConfig cfg =
    { EvalConfig = cfg
      UnwindConfig = unwindConfig cfg }

let trans (cfg: TransConfig) (pm: ProcMap<unit>) (cm: CtorMap) (genv: Env) (s0: State) : (Event * State) list =
    let eval = eval cfg.EvalConfig cm in

    let rec trans s =
        match unwind cfg.UnwindConfig pm cm genv s with
        | Error(err) -> [ (ErrorEvent, ErrorState(format err)) ]
        | Ok(s) ->
            match s with
            | Unwind _ -> failwith "unwind cannot return Unwind"
            | Stop _ -> []
            | Prefix(env, expr, s, line) ->
                match eval env expr with
                | Ok(v) -> [ (Vis(v), s) ]
                | Error err -> [ (ErrorEvent, ErrorState(EvalError.format (EvalError.atLine line err))) ]
            | PrefixRecv(env, expr, var, s, line) ->
                match eval env expr with
                | Ok(VSet vs) ->
                    List.map
                        (fun v ->
                            match bind1 var v s with
                            | Ok(s) -> (Vis v, s)
                            | Error(err) -> (ErrorEvent, ErrorState(EnvError.format (EnvError.atLine line err))))
                        (Set.toList vs)
                | Ok(v) -> [ (ErrorEvent, ErrorState(TransError.format (NotSet(v)))) ]
                | Error err -> [ (ErrorEvent, ErrorState(EvalError.format err)) ]
            | IntCh(s1, s2, _) -> [ (Tau, s1); (Tau, s2) ]
            | ExtCh(s1, s2, line) ->
                (List.map
                    (fun (ev, s1') ->
                        match ev with
                        | Tau -> (Tau, ExtCh(s1', s2, line))
                        | _ -> (ev, s1'))
                    (trans s1))
                @ (List.map
                    (fun (ev, s2') ->
                        match ev with
                        | Tau -> (Tau, ExtCh(s1, s2', line))
                        | _ -> (ev, s2'))
                    (trans s2))
            | Skip _ -> [ (Tick, Omega) ]
            | Seq(s1, s2, line) ->
                let t1 = trans s1 in

                List.fold
                    (fun acc (ev, s1') ->
                        match ev with
                        | Tick ->
                            match s1' with
                            | Omega _ -> (Tau, s2) :: acc
                            | _ -> acc // can happen?
                        | _ -> (ev, Seq(s1', s2, line)) :: acc)
                    []
                    t1
            | If(env, expr, s1, s2, line) ->
                match eval env expr with
                | Ok(VBool true) -> trans s1
                | Ok(VBool false) -> trans s2
                | Ok(v) ->
                    [ (ErrorEvent,
                       ErrorState(EvalError.format (EvalError.atLine line (EvalError.TypeMismatch(v, TBool))))) ]
                | Error(err) -> [ (ErrorEvent, ErrorState(EvalError.format (EvalError.atLine line err))) ]
            | Match(env, expr, sm, line) ->
                match eval env expr with
                | Ok(VUnion(ctor, vs)) ->
                    match Map.tryFind (Some ctor) sm with
                    | Some(varOpts, p2) ->
                        if List.length varOpts = List.length vs then
                            match bindAll (List.zip varOpts vs) p2 with
                            | Ok(p2) -> trans p2
                            | Error(err) -> [ (ErrorEvent, ErrorState(EnvError.format (EnvError.atLine line err))) ]
                        else
                            [ (ErrorEvent,
                               ErrorState(
                                   EvalError.format (
                                       EvalError.UnionValuesLenMismatch(ctor, List.length varOpts, List.length vs)
                                   )
                               )) ]
                    | None ->
                        match Map.tryFind None sm with
                        | Some(varOpts, p2) ->
                            match varOpts with
                            | [ Some var ] ->
                                match bind1 var (VUnion(ctor, vs)) p2 with
                                | Ok(p2) -> trans p2
                                | Error(err) -> [ (ErrorEvent, ErrorState(EnvError.format (EnvError.atLine line err))) ]
                            | [ None ] -> trans p2
                            | _ ->
                                [ (ErrorEvent,
                                   ErrorState(
                                       EvalError.format (
                                           EvalError.atLine line (EvalError.DefaultClauseArgumentLenMustBe1 varOpts)
                                       )
                                   )) ]
                        | None ->
                            [ (ErrorEvent,
                               ErrorState(EvalError.format (EvalError.atLine line (EvalError.NoClauseMatched ctor)))) ]
                | Ok(v) ->
                    [ (ErrorEvent, ErrorState(EvalError.format (EvalError.atLine line (EvalError.ValNotUnion v)))) ]
                | Error(err) -> [ (ErrorEvent, ErrorState(EvalError.format (EvalError.atLine line err))) ]
            | InterfaceParallel(_, Omega, _, Omega, _) -> [ (Tick, Omega) ] // Para6
            | InterfaceParallel(env, p1, expr, p2, line) ->
                let t1 = trans p1 in
                let t2 = trans p2 in

                match eval env expr with
                | Ok(VSet vs) ->
                    (List.fold
                        (fun acc (ev, p1') ->
                            match ev with
                            | Vis ev ->
                                if Set.contains ev vs then
                                    acc
                                else
                                    (Vis ev, InterfaceParallel(env, p1', expr, p2, line)) :: acc // Para1
                            | Tick ->
                                match p1' with
                                | Omega _ -> (Tau, InterfaceParallel(env, p1', expr, p2, line)) :: acc // Para4
                                | _ -> acc
                            | Tau -> (Tau, InterfaceParallel(env, p1', expr, p2, line)) :: acc // Para1
                            | Hid ev' -> (Hid ev', InterfaceParallel(env, p1', expr, p2, line)) :: acc // para1
                            | ErrorEvent -> (ErrorEvent, p1') :: acc)
                        []
                        t1)
                    @ (List.fold
                        (fun acc (ev, p2') ->
                            match ev with
                            | Vis ev ->
                                if Set.contains ev vs then
                                    acc
                                else
                                    (Vis ev, InterfaceParallel(env, p1, expr, p2', line)) :: acc // Para2
                            | Tick ->
                                match p2' with
                                | Omega _ -> (Tau, InterfaceParallel(env, p1, expr, p2', line)) :: acc // Para5
                                | _ -> acc
                            | Tau -> (Tau, InterfaceParallel(env, p1, expr, p2', line)) :: acc // Para2
                            | Hid ev' -> (Hid ev', InterfaceParallel(env, p1, expr, p2', line)) :: acc // para1
                            | ErrorEvent -> (ErrorEvent, p2') :: acc)
                        []
                        t2)
                    @ (List.fold
                        (fun acc ((ev1, s1'), (ev2, s2')) ->
                            match (ev1, ev2) with
                            | Vis ev1, Vis ev2 when ev1 = ev2 ->
                                if Set.contains ev1 vs then
                                    (Vis ev1, InterfaceParallel(env, s1', expr, s2', line)) :: acc // Para3
                                else
                                    acc
                            | ErrorEvent, _ -> (ErrorEvent, s1') :: acc
                            | _, ErrorEvent -> (ErrorEvent, s2') :: acc
                            | _ -> acc)
                        []
                        (List.allPairs t1 t2))
                | Ok(v) ->
                    [ ErrorEvent,
                      ErrorState(EvalError.format (EvalError.atLine line (EvalError.TypeMismatch(v, TSet(TVar 0u))))) ]
                | Error(err) -> [ ErrorEvent, ErrorState(EvalError.format (EvalError.atLine line err)) ]
            | Hide(env, s, expr, line) ->
                match eval env expr with
                | Ok(VSet vs) ->
                    List.map
                        (fun (ev, s') ->
                            match ev with
                            | Vis ev when Set.contains ev vs -> (Hid ev, Hide(env, s', expr, line))
                            | Tick ->
                                match s' with
                                | Omega _ -> (Tick, s')
                                | _ -> (ev, Hide(env, s', expr, line))
                            | ErrorEvent -> (ErrorEvent, s')
                            | _ -> (ev, Hide(env, s', expr, line)))
                        (trans s)
                | Ok(v) ->
                    [ (ErrorEvent,
                       ErrorState(EvalError.format (EvalError.atLine line (EvalError.TypeMismatch(v, TSet(TVar 0u)))))) ]
                | Error(err) -> [ (ErrorEvent, ErrorState(EvalError.format (EvalError.atLine line err))) ]
            | Omega _ -> []
            | ErrorState _ -> []

    trans s0
