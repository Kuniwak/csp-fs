module CSP.Core.State

open FSharpx.Collections
open CSP.Core.ProcMap
open CSP.Core.Type
open CSP.Core.EventSpec
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.Val

type State<'P, 'Ev, 'Ch, 'Var, 'Ctor when 'Ev: comparison and 'Var: comparison and 'Ctor: comparison and 'Ch: comparison>
    =
    | Unwind of Map<'Var, Val<'Ctor>> * 'P * Expr<'Var, 'Ctor> option
    | Stop
    | Skip
    | Prefix of 'Ev * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | PrefixSend of Map<'Var, Val<'Ctor>> * 'Ch * Expr<'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | PrefixRecv of 'Ch * 'Var * Type<'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | IntCh of State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | ExtCh of State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Seq of State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | If of
        Map<'Var, Val<'Ctor>> *
        Expr<'Var, 'Ctor> *
        State<'P, 'Ev, 'Ch, 'Var, 'Ctor> *
        State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Match of
        Map<'Var, Val<'Ctor>> *
        Expr<'Var, 'Ctor> *
        Map<Ctor<'Ctor>, 'Var * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>> *
        ('Var * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) option
    | InterfaceParallel of
        State<'P, 'Ev, 'Ch, 'Var, 'Ctor> *
        Set<EventSpec<'Ev, 'Ch>> *
        State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Hide of State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Set<EventSpec<'Ev, 'Ch>>
    | Omega
    | Error of string * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>

let rec bind (var: 'Var) (v: Val<'Ctor>) (s: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) =
    match s with
    | Unwind(env, n, eOpt) -> Unwind(Map.add var v env, n, eOpt)
    | Stop -> Stop
    | Skip -> Skip
    | Prefix(ev, s') -> Prefix(ev, bind var v s')
    | PrefixSend(env, ch, e, s') -> PrefixSend(Map.add var v env, ch, e, bind var v s')
    | PrefixRecv(ch, v', t, s') -> PrefixRecv(ch, v', t, bind var v s')
    | IntCh(s1, s2) -> IntCh(bind var v s1, bind var v s2)
    | ExtCh(s1, s2) -> ExtCh(bind var v s1, bind var v s2)
    | Seq(s1, s2) -> Seq(bind var v s1, bind var v s2)
    | If(env, e, s1, s2) -> If(Map.add var v env, e, bind var v s1, bind var v s2)
    | Match(env, e, sm, ds) ->
        Match(
            Map.add var v env,
            e,
            Map.map (fun _ (var', s) -> (var', bind var v s)) sm,
            Option.map (fun (var', s) -> (var', bind var v s)) ds
        )
    | InterfaceParallel(s1, evs, s2) -> InterfaceParallel(bind var v s1, evs, bind var v s2)
    | Hide(s, evs) -> Hide(bind var v s, evs)
    | Omega -> Omega
    | Error _ -> s

let rec ofProc
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (env: Map<'Var, Val<'Ctor>>)
    (p: Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    : State<'P, 'Ev, 'Ch, 'Var, 'Ctor> =
    match p with
    | Proc.Unwind(n, e) -> Unwind(env, n, e)
    | Proc.Stop -> Stop
    | Proc.Skip -> Skip
    | Proc.Prefix(ev, p') -> Prefix(ev, ofProc m env p')
    | Proc.PrefixSend(ch, e, p') -> PrefixSend(env, ch, e, ofProc m env p')
    | Proc.PrefixRecv(ch, v, t, p') -> PrefixRecv(ch, v, t, ofProc m env p')
    | Proc.IntCh(p1, p2) -> IntCh(ofProc m env p1, ofProc m env p2)
    | Proc.ExtCh(p1, p2) -> ExtCh(ofProc m env p1, ofProc m env p2)
    | Proc.Seq(p1, p2) -> Seq(ofProc m env p1, ofProc m env p2)
    | Proc.If(e, p1, p2) -> If(env, e, ofProc m env p1, ofProc m env p2)
    | Proc.Match(e, pm, dp) ->
        Match(
            env,
            e,
            Map.map (fun _ (v, p) -> (v, ofProc m env p)) pm,
            Option.map (fun (v, p) -> (v, ofProc m env p)) dp
        )
    | Proc.InterfaceParallel(p1, evs, p2) -> InterfaceParallel(ofProc m env p1, evs, ofProc m env p2)
    | Proc.Interleave(p1, p2) -> InterfaceParallel(ofProc m env p1, Set.empty, ofProc m env p2)
    | Proc.Hide(p, evs) -> Hide(ofProc m env p, evs)
    | Proc.Guard(e, p) -> If(env, e, ofProc m env p, Stop)

let unwind
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (s0: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    : State<'P, 'Ev, 'Ch, 'Var, 'Ctor> =
    let rec loop s visited =
        match s with
        | Unwind(env, n, eOpt) ->
            if Set.contains s visited then
                failwith "circular definition"
            else
                match Map.tryFind n m with
                | Some t ->
                    match (t, eOpt) with
                    | (Some var, p), Some e ->
                        let env = Map.add var (eval env e) env in loop (ofProc m env p) (Set.add s visited)
                    | (None, p), None -> loop (ofProc m env p) (Set.add s visited)
                    | (None, _), Some _ -> Error("given a value to Unwind, but not needed at unwind", s)
                    | (Some _, _), None -> Error("needed a value by Unwind, but not given at unwind", s)
                | None ->
                    let sep = ", " in

                    let ms =
                        String.concat
                            sep
                            (Seq.map
                                (fun (n, (varOpt, _)) ->
                                    match varOpt with
                                    | Some var -> $"{n} {var}"
                                    | None -> $"{n}")
                                (Map.toList m)) in

                    failwith $"no such process: {n} in [{ms}]"
        | _ -> s

    loop s0 Set.empty

let format (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>) (s0: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) : string =
    let rec f s isTop =
        match s with
        | Unwind(env, n, eOpt) ->
            if isTop then
                f (unwind m s) false
            else
                match eOpt with
                | Some e -> $"{n} {Expr.format e} env={Env.format env}"
                | None -> $"{n}"
        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(ev, s') -> $"({ev} -> {f s' false})"
        | PrefixSend(env, ev, e, s') -> $"({ev}!{Expr.format e} -> {f s' false} env={Env.format env})"
        | PrefixRecv(ev, var, t, s') -> $"({ev}?({var}: {Type.format t}) -> {f s' false})"
        | IntCh(s1, s2) -> $"({f s1 false} ⨅ {f s2 false})"
        | ExtCh(s1, s2) -> $"({f s1 false} □ {f s2 false})"
        | Seq(s1, s2) -> $"({f s1 false} ; {f s2 false})"
        | If(env, e, s1, s2) -> $"(if {Expr.format e} then {f s1 false} else {f s2 false}) env={Env.format env})"
        | Match(env, e, sm, ds) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, p')) -> $"{c} {v} -> {f p' false}") (Map.toList sm) in

            match ds with
            | Some(v, p') ->
                $"(match {Expr.format e} with {String.concat sep cs'} | {v} -> {f p' false} env={Env.format env})"
            | None -> $"(match {Expr.format e} with {String.concat sep cs'} env={Env.format env})"
        | InterfaceParallel(s1, evs, s2) -> $"({f s1 false} ⟦{EventSpec.format evs}⟧ {f s2 false})"
        | Hide(s, evs) -> $"({f s false} \\\\ {EventSpec.format evs})"
        | Omega -> "Ω"
        | Error(msg, s) -> $"(ERROR: {msg} at {f s false})"

    f s0 true
