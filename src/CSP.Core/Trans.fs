module CSP.Core.Trans

open CSP.Core.Env
open CSP.Core.LineNum
open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.Event
open CSP.Core.Val
open CSP.Core.Type
open CSP.Core.EvalError
open CSP.Core.Expr
open CSP.Core.State
open CSP.Core.Eval

let init (pm: ProcMap) (genv: Env) (pn: ProcId) (vOpt: Val option) : State =
    match Map.tryFind pn pm with
    | None -> failwith $"no such process: {pn}"
    | Some(var, _) ->
        let genv, varOpt =
            match (var, vOpt) with
            | Some var, Some v ->
                match Env.bind1 var v genv with
                | Ok(env) -> (env, Some var)
                | Error(err) -> failwith $"error: %s{EnvError.format err}"
            | None, None -> (genv, None)
            | None, Some _ -> failwith "given a value to Unwind, but not needed at init"
            | Some _, None -> failwith "needed a value by Unwind, but not given at init"

        ofProc genv (Proc.Unwind(pn, Option.map (fun var -> VarRef(var, (), unknown)) varOpt, unknown))

type TransConfig =
    { EvalConfig: EvalConfig
      UnwindConfig: UnwindConfig }

let transConfig cfg =
    { EvalConfig = cfg
      UnwindConfig = unwindConfig cfg }

let trans (cfg: TransConfig) (pm: ProcMap) (cm: CtorMap) (genv: Env) (s0: State) : (Event * State) list =
    let eval = eval cfg.EvalConfig cm in

    let rec trans s =
        match unwind cfg.UnwindConfig pm cm genv s with
        | Unwind _ -> failwith "unwind cannot return Unwind"
        | Stop _ -> []
        | Prefix(env, expr, s) ->
            match eval env expr with
            | Ok(v) -> [ (Vis(v), s) ]
            | Error err -> [ (ErrorEvent, ErrorState(EvalError.format err, s)) ]
        | PrefixRecv(env, expr, var, s) ->
            match eval env expr with
            | Ok(VSet vs) ->
                List.map
                    (fun v ->
                        match bind1 var v s with
                        | Ok(s) -> (Vis v, s)
                        | Error(err) -> (ErrorEvent, ErrorState(EnvError.format err, s)))
                    (Set.toList vs)
            | Ok(v) -> [ (ErrorEvent, ErrorState(EvalError.format (TypeMismatch(v, TSet(TVar 0u))), s)) ]
            | Error err -> [ (ErrorEvent, ErrorState(EvalError.format err, s)) ]
        | IntCh(s1, s2) -> [ (Tau, s1); (Tau, s2) ]
        | ExtCh(s1, s2) ->
            (List.map
                (fun (ev, s1') ->
                    match ev with
                    | Tau -> (Tau, ExtCh(s1', s2))
                    | _ -> (ev, s1'))
                (trans s1))
            @ (List.map
                (fun (ev, s2') ->
                    match ev with
                    | Tau -> (Tau, ExtCh(s1, s2'))
                    | _ -> (ev, s2'))
                (trans s2))
        | Skip -> [ (Tick, Omega) ]
        | Seq(s1, s2) ->
            let t1 = trans s1 in

            List.fold
                (fun acc (ev, s1') ->
                    match ev with
                    | Tick ->
                        match s1' with
                        | Omega _ -> (Tau, s2) :: acc
                        | _ -> acc // can happen?
                    | _ -> (ev, Seq(s1', s2)) :: acc)
                []
                t1
        | If(env, expr, s1, s2) ->
            match eval env expr with
            | Ok(VBool true) -> trans s1
            | Ok(VBool false) -> trans s2
            | Ok(v) -> [ (ErrorEvent, ErrorState(EvalError.format (TypeMismatch(v, TBool)), s)) ]
            | Error(err) -> [ (ErrorEvent, ErrorState(EvalError.format err, s)) ]
        | Match(env, expr, sm, ds) ->
            match eval env expr with
            | Ok(VUnion(ctor, vs)) ->
                match Map.tryFind ctor sm with
                | Some(vars, p2) ->
                    if List.length vars = List.length vs then
                        match bindAll (List.zip vars vs) p2 with
                        | Ok(p2) -> trans p2
                        | Error(err) -> [ (ErrorEvent, ErrorState(EnvError.format err, s)) ]
                    else
                        [ (ErrorEvent,
                           ErrorState(
                               EvalError.format (UnionValuesLenMismatch(ctor, List.length vars, List.length vs)),
                               s
                           )) ]
                | None ->
                    match ds with
                    | Some(Some var, p2) ->
                        match bind1 var (VUnion(ctor, vs)) p2 with
                        | Ok(p2) -> trans p2
                        | Error(err) -> [ (ErrorEvent, ErrorState(EnvError.format err, s)) ]
                    | Some(None, p2) -> trans p2
                    | None -> [ (ErrorEvent, ErrorState(EvalError.format (NoClauseMatched ctor), s)) ]
            | Ok(v) -> [ (ErrorEvent, ErrorState(EvalError.format (ValNotUnion v), s)) ]
            | Error(err) -> [ (ErrorEvent, ErrorState(EvalError.format err, s)) ]
        | InterfaceParallel(_, Omega, _, Omega) -> [ (Tick, Omega) ] // Para6
        | InterfaceParallel(env, p1, expr, p2) ->
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
                                (Vis ev, InterfaceParallel(env, p1', expr, p2)) :: acc // Para1
                        | Tick ->
                            match p1' with
                            | Omega _ -> (Tau, InterfaceParallel(env, p1', expr, p2)) :: acc // Para4
                            | _ -> acc
                        | Tau -> (Tau, InterfaceParallel(env, p1', expr, p2)) :: acc // Para1
                        | Hid ev' -> (Hid ev', InterfaceParallel(env, p1', expr, p2)) :: acc // para1
                        | ErrorEvent -> (ErrorEvent, ErrorState("lhs error", p1')) :: acc)
                    []
                    t1)
                @ (List.fold
                    (fun acc (ev, p2') ->
                        match ev with
                        | Vis ev ->
                            if Set.contains ev vs then
                                acc
                            else
                                (Vis ev, InterfaceParallel(env, p1, expr, p2')) :: acc // Para2
                        | Tick ->
                            match p2' with
                            | Omega _ -> (Tau, InterfaceParallel(env, p1, expr, p2')) :: acc // Para5
                            | _ -> acc
                        | Tau -> (Tau, InterfaceParallel(env, p1, expr, p2')) :: acc // Para2
                        | Hid ev' -> (Hid ev', InterfaceParallel(env, p1, expr, p2')) :: acc // para1
                        | ErrorEvent -> (ErrorEvent, ErrorState("rhs error", p2')) :: acc)
                    []
                    t2)
                @ (List.fold
                    (fun acc ((ev1, s1'), (ev2, s2')) ->
                        match (ev1, ev2) with
                        | Vis ev1, Vis ev2 when ev1 = ev2 ->
                            if Set.contains ev1 vs then
                                (Vis ev1, InterfaceParallel(env, s1', expr, s2')) :: acc // Para3
                            else
                                acc
                        | ErrorEvent, _ -> (ErrorEvent, ErrorState("lhs error", s1')) :: acc
                        | _, ErrorEvent -> (ErrorEvent, ErrorState("rhs error", s2')) :: acc
                        | _ -> acc)
                    []
                    (List.allPairs t1 t2))
            | Ok(v) -> [ ErrorEvent, ErrorState(EvalError.format (TypeMismatch(v, TSet(TVar 0u))), s) ]
            | Error(err) -> [ ErrorEvent, ErrorState(EvalError.format err, s) ]
        | Hide(env, s, expr) ->
            match eval env expr with
            | Ok(VSet vs) ->
                List.map
                    (fun (ev, s') ->
                        match ev with
                        | Vis ev when Set.contains ev vs -> (Hid ev, Hide(env, s', expr))
                        | Tick ->
                            match s' with
                            | Omega _ -> (Tick, s')
                            | _ -> (ev, Hide(env, s', expr))
                        | ErrorEvent -> (ErrorEvent, ErrorState("error in Hide", s))
                        | _ -> (ev, Hide(env, s', expr)))
                    (trans s)
            | Ok(v) -> [ (ErrorEvent, ErrorState(EvalError.format (TypeMismatch(v, TSet(TVar 0u))), s)) ]
            | Error(err) -> [ (ErrorEvent, ErrorState(EvalError.format err, s)) ]
        | Omega _ -> []
        | ErrorState _ -> []

    trans s0
