module CSP.Core.Trans

open CSP.Core.ProcMap
open CSP.Core.Val
open CSP.Core.Type
open CSP.Core.Event
open CSP.Core.EventSpec
open CSP.Core.Expr
open CSP.Core.State

let init
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (genv: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : State<'P, 'Ev, 'Ch, 'Var, 'Ctor> =
    let env, varOpt =
        match (Map.find n m, vOpt) with
        | (Some var, _), Some v -> (Map.add var v genv, Some var)
        | (None, _), None -> (genv, None)
        | (None, _), Some _ -> failwith "given a value to Unwind, but not needed at init"
        | (Some _, _), None -> failwith "needed a value by Unwind, but not given at init"

    ofProc m env (Proc.Unwind(n, Option.map VarRef varOpt))

let rec trans
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (genv: Map<'Var, Val<'Ctor>>)
    (s0: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    : (Event<'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) list =
    match unwind m s0 with
    | Unwind _ -> failwith "unwind cannot return Unwind"
    | Stop _ -> []
    | Prefix(ev, s) -> [ (Vis ev, s) ]
    | PrefixSend(env, ch, e, s) -> [ (VisChan(ch, eval env e), s) ]
    | PrefixRecv(ch, var, t, s) -> List.map (fun v -> (VisChan(ch, v), bind var v s) ) (univ t)
    | IntCh(s1, s2) -> [ (Tau, s1); (Tau, s2) ]
    | ExtCh(s1, s2) ->
        (List.fold
            (fun acc (ev, s1') ->
                match ev with
                | Tau -> (Tau, ExtCh(s1', s2)) :: acc
                | _ -> (ev, s1') :: acc)
            []
            (trans m genv s1))
        @ (List.fold
            (fun acc (ev, s2') ->
                match ev with
                | Tau -> (Tau, ExtCh(s1, s2')) :: acc
                | _ -> (ev, s2') :: acc)
            []
            (trans m genv s2))
    | Skip -> [ (Tick, Omega) ]
    | Seq(s1, s2) ->
        let t1 = trans m genv s1 in

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
    | If(env, e, s1, s2) ->
        match eval env e with
        | VBool true -> trans m genv s1
        | VBool false -> trans m genv s2
        | v -> [ (Event.Error, Error($"passed a not boolean value to If: eval env {e} = {v} (env = {env})", s0)) ]
    | Match(env, e, sm, ds) ->
        match eval env e with
        | VUnion(c, v) ->
            match Map.tryFind c sm with
            | Some(var, p2) -> trans m genv (bind var v p2)
            | None ->
                match ds with
                | Some(var, p2) -> trans m genv (bind var (VUnion(c, v)) p2)
                | None -> [ (Event.Error, Error($"Match are not exhausted: | {c} {v} -> ... ", s0)) ]
        | v -> [ (Event.Error, Error($"passed a not union value to Match: eval env {e} = {v} (env = {env})", s0)) ]
    | InterfaceParallel(Omega, _, Omega) -> [ (Tick, Omega) ] // Para6
    | InterfaceParallel(p1, evs, p2) ->
        let t1 = trans m genv p1 in
        let t2 = trans m genv p2 in

        (List.fold
            (fun acc (ev, p1') ->
                match ev with
                | Vis ev' ->
                    if Set.contains (Event ev') evs then
                        acc
                    else
                        (Vis ev', InterfaceParallel(p1', evs, p2)) :: acc // Para1
                | VisChan(ch', v) ->
                    if Set.contains (Chan ch') evs then
                        acc
                    else
                        (VisChan(ch', v), InterfaceParallel(p1', evs, p2)) :: acc // Para1
                | Tick ->
                    match p1' with
                    | Omega _ -> (Tau, InterfaceParallel(p1', evs, p2)) :: acc // Para4
                    | _ -> acc
                | Tau -> (Tau, InterfaceParallel(p1', evs, p2)) :: acc // Para1
                | Hid ev' -> (Hid ev', InterfaceParallel(p1', evs, p2)) :: acc // para1
                | HidChan(ch', v) -> (HidChan(ch', v), InterfaceParallel(p1', evs, p2)) :: acc // Para1
                | Event.Error -> (Event.Error, Error("lhs error", s0)) :: acc)
            []
            t1)
        @ (List.fold
            (fun acc (ev, p2') ->
                match ev with
                | Vis ev' ->
                    if Set.contains (Event ev') evs then
                        acc
                    else
                        (Vis ev', InterfaceParallel(p1, evs, p2')) :: acc // Para2
                | VisChan(ch', v) ->
                    if Set.contains (Chan ch') evs then
                        acc
                    else
                        (VisChan(ch', v), InterfaceParallel(p1, evs, p2')) :: acc
                | Tick ->
                    match p2' with
                    | Omega _ -> (Tau, InterfaceParallel(p1, evs, p2')) :: acc // Para5
                    | _ -> acc
                | Tau -> (Tau, InterfaceParallel(p1, evs, p2')) :: acc // Para2
                | Hid ev' -> (Hid ev', InterfaceParallel(p1, evs, p2')) :: acc // para1
                | HidChan(ch', v) -> (HidChan(ch', v), InterfaceParallel(p1, evs, p2')) :: acc // Para1
                | Event.Error -> (Event.Error, Error("rhs error", s0)) :: acc)
            []
            t2)
        @ (List.fold
            (fun acc ((ev1, s1'), (ev2, s2')) ->
                match (ev1, ev2) with
                | Vis ev1', Vis ev2' when ev1' = ev2' ->
                    if Set.contains (Event ev1') evs then
                        (ev1, InterfaceParallel(s1', evs, s2')) :: acc // Para3
                    else
                        acc
                | VisChan(ch1, v1), VisChan(ch2, v2) when ch1 = ch2 && v1 = v2 ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, v1), InterfaceParallel(s1', evs, s2'))
                        :: acc // Para3
                    else
                        acc
                | Event.Error, Event.Error -> (Event.Error, Error("both lhs and rhs errors", s0)) :: acc
                | Event.Error, _ -> (Event.Error, Error("lhs error", s0)) :: acc
                | _, Event.Error -> (Event.Error, Error("rhs error", s0)) :: acc
                | _ -> acc)
            []
            (List.allPairs t1 t2))
    | Hide(s, evs) ->
        List.map
            (fun (ev, s') ->
                match ev with
                | Vis e when Set.contains (Event e) evs -> (Hid e, Hide(s', evs))
                | VisChan(ch, v) when Set.contains (Chan ch) evs -> (HidChan(ch, v), Hide(s', evs))
                | Tick ->
                    match s' with
                    | Omega _ -> (Tick, s')
                    | _ -> (ev, Hide(s', evs))
                | Event.Error -> (Event.Error, Error("error in Hide", s0))
                | _ -> (ev, Hide(s', evs)))
            (trans m genv s)
    | Omega _ -> []
    | Error _ -> [ (Event.Error, s0) ]
