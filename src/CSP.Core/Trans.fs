module CSP.Core.Trans

open CSP.Core.CtorMap
open CSP.Core.ProcMap
open CSP.Core.Val
open CSP.Core.Expr
open CSP.Core.State

let init
    (m: ProcMap<'P, 'Var, 'Ctor>)
    (genv: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : State<'P, 'Var, 'Ctor> =
    let env, varOpt =
        match (Map.find n m, vOpt) with
        | (Some var, _), Some v -> (Map.add var v genv, Some var)
        | (None, _), None -> (genv, None)
        | (None, _), Some _ -> failwith "given a value to Unwind, but not needed at init"
        | (Some _, _), None -> failwith "needed a value by Unwind, but not given at init"

    ofProc m env (Proc.Unwind(n, Option.map VarRef varOpt))

let rec trans
    (pm: ProcMap<'P, 'Var, 'Ctor>)
    (cm: CtorMap<'Ctor>)
    (env0: Map<'Var, Val<'Ctor>>)
    (s0: State<'P, 'Var, 'Ctor>)
    : (Event<'Ctor> * State<'P, 'Var, 'Ctor>) list =
    match unwind pm cm s0 with
    | Unwind _ -> failwith "unwind cannot return Unwind"
    | Stop _ -> []
    | Prefix(env, expr, s) -> [ (Vis(eval cm env expr), s) ]
    | PrefixRecv(env, expr, var, s) ->
        match eval cm env expr with
        | VSet vs -> List.map (fun v -> (Vis v, bind var v s)) (Set.toList vs)
        | v ->
            [ (Val.Error,
               Error(
                   $"PrefixRecv expected a set, but got: expr env {Expr.format expr} = {Val.format v} (env={Env.format env})",
                   s
               )) ]
    | IntCh(s1, s2) -> [ (Tau, s1); (Tau, s2) ]
    | ExtCh(s1, s2) ->
        (List.fold
            (fun acc (ev, s1') ->
                match ev with
                | Tau -> (Tau, ExtCh(s1', s2)) :: acc
                | _ -> (ev, s1') :: acc)
            []
            (trans pm cm env0 s1))
        @ (List.fold
            (fun acc (ev, s2') ->
                match ev with
                | Tau -> (Tau, ExtCh(s1, s2')) :: acc
                | _ -> (ev, s2') :: acc)
            []
            (trans pm cm env0 s2))
    | Skip -> [ (Tick, Omega) ]
    | Seq(s1, s2) ->
        let t1 = trans pm cm env0 s1 in

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
        match eval cm env expr with
        | VBool true -> trans pm cm env s1
        | VBool false -> trans pm cm env s2
        | v -> [ (Val.Error, Error($"expected a boolean value but got: eval env {expr} = {v} (env = {env})", s0)) ]
    | Match(env, expr, sm, ds) ->
        match eval cm env expr with
        | VUnion(c, v) ->
            match Map.tryFind c sm with
            | Some(var, p2) -> trans pm cm env (bind var v p2)
            | None ->
                match ds with
                | Some(Some var, p2) -> trans pm cm env (bind var (VUnion(c, v)) p2)
                | Some(None, p2) -> trans pm cm env p2
                | None -> [ (Val.Error, Error($"Match are not exhausted: | {c} {v} -> ... ", s0)) ]
        | v -> [ (Val.Error, Error($"Match expected an union value but got: eval env {expr} = {v} (env = {env})", s0)) ]
    | InterfaceParallel(_, Omega, _, Omega) -> [ (Tick, Omega) ] // Para6
    | InterfaceParallel(env, p1, expr, p2) ->
        let t1 = trans pm cm env p1 in
        let t2 = trans pm cm env p2 in

        match eval cm env expr with
        | VSet vs ->
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
                    | Event.Error -> (Val.Error, Error("lhs error", s0)) :: acc)
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
                    | Event.Error -> (Val.Error, Error("rhs error", s0)) :: acc)
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
                    | Event.Error, Event.Error -> (Val.Error, Error("both lhs and rhs errors", s0)) :: acc
                    | Event.Error, _ -> (Val.Error, Error("lhs error", s0)) :: acc
                    | _, Event.Error -> (Val.Error, Error("rhs error", s0)) :: acc
                    | _ -> acc)
                []
                (List.allPairs t1 t2))
        | v ->
            [ Val.Error,
              Error(
                  $"InterfaceParallel expected a set, but got: eval env {Expr.format expr} = {Val.format v} (env = {Env.format env})",
                  s0
              ) ]
    | Hide(env, s, expr) ->
        match eval cm env expr with
        | VSet vs ->
            List.map
                (fun (ev, s') ->
                    match ev with
                    | Vis ev when Set.contains ev vs -> (Hid ev, Hide(env, s', expr))
                    | Tick ->
                        match s' with
                        | Omega _ -> (Tick, s')
                        | _ -> (ev, Hide(env, s', expr))
                    | Event.Error -> (Val.Error, Error("error in Hide", s0))
                    | _ -> (ev, Hide(env, s', expr)))
                (trans pm cm env0 s)
        | v ->
            [ (Val.Error,
               Error(
                   $"Hide expected a set, but got: expr env {Expr.format expr} = {Val.format v} (env = {Env.format env})",
                   s0
               )) ]
    | Omega _ -> []
    | Error _ -> [ (Val.Error, s0) ]
