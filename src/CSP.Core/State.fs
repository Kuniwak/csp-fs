module CSP.Core.State

open CSP.Core.ProcMap
open FSharpx.Collections
open CSP.Core.Event
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
    | PrefixRecv of 'Ch * 'Var * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
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

let merge (m1: Map<'k, 'v>) (m2: Map<'k, 'v>) : Map<'k, 'v> =
    Map.fold
        (fun acc k v2 ->
            match Map.tryFind k acc with
            | Some v1 when v1 = v2 -> acc
            | Some v1 -> failwith $"merge failed because of not disjoint: {v1} <> {v2} at {k}"
            | None -> Map.add k v2 acc)
        m1
        m2

let rec bind (var: 'Var) (v: Val<'Ctor>) (s: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) =
    match s with
    | Unwind(env, n, eOpt) -> Unwind(Map.add var v env, n, eOpt)
    | Stop -> Stop
    | Skip -> Skip
    | Prefix(ev, s') -> Prefix(ev, bind var v s')
    | PrefixSend(env, ch, e, s') -> PrefixSend(Map.add var v env, ch, e, bind var v s')
    | PrefixRecv(ch, v', s') -> PrefixRecv(ch, v', bind var v s')
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
    | Proc.PrefixRecv(ch, v, p') -> PrefixRecv(ch, v, ofProc m env p')
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
                match (Map.find n m, eOpt) with
                | (Some var, p), Some e ->
                    let env = Map.add var (eval env e) env in loop (ofProc m env p) (Set.add s visited)
                | (None, p), None -> loop (ofProc m env p) (Set.add s visited)
                | (None, _), Some _ -> Error("given a value to Unwind, but not needed at unwind", s)
                | (Some _, _), None -> Error("needed a value by Unwind, but not given at unwind", s)
        | _ -> s

    loop s0 Set.empty

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
    | PrefixRecv(ch, v, s) -> [ (VisRecv(ch, v), s) ]
    | IntCh(s1, s2) -> [ (Tau, s1); (Tau, s2) ]
    | ExtCh(s1, s2) ->
        let t1 = trans m genv s1 in
        let t2 = trans m genv s2 in

        (List.filter (fun (ev, _) -> ev <> Tau) t1)
        @ (List.filter (fun (ev, _) -> ev <> Tau) t2)
        @ (List.fold
            (fun acc (ev, s1') ->
                match ev with
                | Tau -> (Tau, ExtCh(s1', s2)) :: acc
                | Event.Error -> (Event.Error, Error("lhs error in ExtCh", s0)) :: acc
                | _ -> acc)
            []
            t1)
        @ (List.fold
            (fun acc (ev, s2') ->
                match ev with
                | Tau -> (Tau, ExtCh(s1, s2')) :: acc
                | Event.Error -> (Event.Error, Error("rhs error in ExtCh", s0)) :: acc
                | _ -> acc)
            []
            t2)
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
                | Event.Error -> (Event.Error, Error("lhs error in Seq", s0)) :: acc
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
                | VisRecv(ch', v) ->
                    if Set.contains (Chan ch') evs then
                        acc
                    else
                        (VisRecv(ch', v), InterfaceParallel(p1', evs, p2)) :: acc // Para1
                | Tick ->
                    match p1' with
                    | Omega _ -> (Tau, InterfaceParallel(p1', evs, p2)) :: acc // Para4
                    | _ -> acc
                | Tau -> (Tau, InterfaceParallel(p1', evs, p2)) :: acc // Para1
                | Hid ev' -> (Hid ev', InterfaceParallel(p1', evs, p2)) :: acc // para1
                | HidChan(ch', v) -> (HidChan(ch', v), InterfaceParallel(p1', evs, p2)) :: acc // Para1
                | HidRecv(ch', v) -> (HidRecv(ch', v), InterfaceParallel(p1', evs, p2)) :: acc // Para1
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
                | VisRecv(ch', v) ->
                    if Set.contains (Chan ch') evs then
                        acc
                    else
                        (VisRecv(ch', v), InterfaceParallel(p1, evs, p2')) :: acc // Para1
                | Tick ->
                    match p2' with
                    | Omega _ -> (Tau, InterfaceParallel(p1, evs, p2')) :: acc // Para5
                    | _ -> acc
                | Tau -> (Tau, InterfaceParallel(p1, evs, p2')) :: acc // Para2
                | Hid ev' -> (Hid ev', InterfaceParallel(p1, evs, p2')) :: acc // para1
                | HidChan(ch', v) -> (HidChan(ch', v), InterfaceParallel(p1, evs, p2')) :: acc // Para1
                | HidRecv(ch', v) -> (HidRecv(ch', v), InterfaceParallel(p1, evs, p2')) :: acc // Para1
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
                | VisChan(ch1, v1), VisChan(ch2, v2) when ch1 = ch2 && (v1 = v2 || v1 = VAny || v2 = VAny) ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, (if v1 = VAny then v2 else v1)), InterfaceParallel(s1', evs, s2'))
                        :: acc // Para3
                    else
                        acc
                | VisChan(ch1, v), VisRecv(ch2, var) when ch1 = ch2 ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, v), InterfaceParallel(s1', evs, bind var v s2')) :: acc // Para3
                    else
                        acc
                | VisRecv(ch1, var), VisChan(ch2, v) when ch1 = ch2 ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, v), InterfaceParallel(bind var v s1', evs, s2')) :: acc // Para3
                    else
                        acc
                | VisRecv(ch1, _), VisRecv(ch2, _) when ch1 = ch2 ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, VAny), InterfaceParallel(s1', evs, s2')) :: acc // Para3
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
                | VisRecv(ch, v) when Set.contains (Chan ch) evs -> (HidRecv(ch, v), Hide(s', evs))
                | Tick ->
                    match s' with
                    | Omega _ -> (Tick, s')
                    | _ -> (ev, Hide(s', evs))
                | Event.Error -> (Event.Error, Error("error in Hide", s0))
                | _ -> (ev, Hide(s', evs)))
            (trans m genv s)
    | Omega _ -> []
    | Error _ -> [ (Event.Error, s0) ]

let bfs
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (genv: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    (f:
        State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
            -> (Event<'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) list
            -> State<'P, 'Ev, 'Ch, 'Var, 'Ctor> list
            -> Unit)
    =
    let mutable q: Queue<State<'P, 'Ev, 'Ch, 'Var, 'Ctor> list> =
        Queue.conj [ init m genv n vOpt ] Queue.empty in

    while not (Queue.isEmpty q) do
        let path, q' = Queue.uncons q in
        q <- q'
        let s = List.head path in
        let t = trans m genv s in
        f s t (List.rev path)

        List.iter
            (fun p' ->
                let u = unwind m p' in

                if not (List.contains u path) then
                    q <- Queue.conj (u :: path) q)
            (List.map (fun (_, p') -> p') t)

let deadlocks
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (env: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : State<'P, 'Ev, 'Ch, 'Var, 'Ctor> list list =
    let mutable r: State<'P, 'Ev, 'Ch, 'Var, 'Ctor> list list = [] in

    bfs m env n vOpt (fun _ t path ->
        if List.isEmpty t then
            r <- path :: r)

    r

let edges
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (genv: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : (State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Event<'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) list =
    let rec loop s visited =
        List.fold
            (fun (rAcc, sAcc) (s1, ev, s1') ->
                if Set.contains (unwind m s1') sAcc then
                    ((s1, ev, s1') :: rAcc, sAcc)
                else
                    let rAcc', sAcc' = loop s1' (Set.add (unwind m s1) sAcc)
                    ((s1, ev, s1') :: rAcc @ rAcc', sAcc'))
            ([], Set.add (unwind m s) visited)
            (List.map (fun (ev, s') -> (s, ev, s')) (trans m genv s))

    fst (loop (init m genv n vOpt) Set.empty)

let format (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>) (s0: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) : string =
    let rec f s isTop =
        match s with
        | Unwind(_, n, eOpt) ->
            if isTop then
                f (unwind m s) false
            else
                match eOpt with
                | Some e -> $"{n} {Expr.format e}"
                | None -> $"{n}"
        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(ev, s') -> $"({ev} -> {f s' false})"
        | PrefixSend(env, ev, e, s') -> $"({ev}!{Expr.format e} -> {f s' false} env={Env.format env})"
        | PrefixRecv(ev, var, s') -> $"({ev}?{var} -> {f s' false})"
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

let dot
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (env: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : string =
    let dq = "\""
    let sq = "'"

    let r' =
        List.map
            (fun (s, ev, s') ->
                $"  \"{(format m s).Replace(dq, sq)}\" -> \"{(format m s').Replace(dq, sq)}\" [label=\"{(Event.format ev).Replace(dq, sq)}\"]")
            (edges m env n vOpt)

    let sep = "\n"

    $"""digraph G {{
{String.concat sep r'}
}}"""
