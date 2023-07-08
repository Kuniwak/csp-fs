module CSP.Core.State

open CSP.Core.ProcMap
open FSharpx.Collections
open CSP.Core.Event
open CSP.Core.Env
open CSP.Core.EventSpec
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.Val

type State<'P, 'Ev, 'Ch, 'Var, 'Ctor when 'Ev: comparison and 'Var: comparison and 'Ctor: comparison and 'Ch: comparison>
    =
    | Unwind of Map<'Var, Val<'Ctor>> * 'P * Expr<'Var, 'Ctor> option
    | Stop of Map<'Var, Val<'Ctor>>
    | Skip of Map<'Var, Val<'Ctor>>
    | Prefix of Map<'Var, Val<'Ctor>> * 'Ev * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | PrefixSend of Map<'Var, Val<'Ctor>> * 'Ch * Expr<'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | PrefixRecv of Map<'Var, Val<'Ctor>> * 'Ch * 'Var * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | IntCh of Map<'Var, Val<'Ctor>> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | ExtCh of Map<'Var, Val<'Ctor>> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Seq of Map<'Var, Val<'Ctor>> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
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
        Map<'Var, Val<'Ctor>> *
        State<'P, 'Ev, 'Ch, 'Var, 'Ctor> *
        Set<EventSpec<'Ev, 'Ch>> *
        State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
    | Hide of Map<'Var, Val<'Ctor>> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Set<EventSpec<'Ev, 'Ch>>
    | Omega of Map<'Var, Val<'Ctor>>
    | Error of string * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>

let rec ofEnv (s: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) : Env<'Var, 'Ctor> =
    match s with
    | Unwind(env, _, _) -> env
    | Stop env -> env
    | Skip env -> env
    | Prefix(env, _, _) -> env
    | PrefixSend(env, _, _, _) -> env
    | PrefixRecv(env, _, _, _) -> env
    | IntCh(env, _, _) -> env
    | ExtCh(env, _, _) -> env
    | Seq(env, _, _) -> env
    | If(env, _, _, _) -> env
    | Match(env, _, _, _) -> env
    | InterfaceParallel(env, _, _, _) -> env
    | Hide(env, _, _) -> env
    | Omega env -> env
    | Error(_, s') -> ofEnv s'

let rec bind (var: 'Var) (v: Val<'Ctor>) (s: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) =
    match s with
    | Unwind(env, n, eOpt) -> Unwind(Map.add var v env, n, eOpt)
    | Stop env -> Stop(Map.add var v env)
    | Skip env -> Skip(Map.add var v env)
    | Prefix(env, ev, s') -> Prefix(Map.add var v env, ev, bind var v s')
    | PrefixSend(env, ch, v', s') -> PrefixSend(Map.add var v env, ch, v', bind var v s')
    | PrefixRecv(env, ch, v', s') -> PrefixRecv(Map.add var v env, ch, v', bind var v s')
    | IntCh(env, s1, s2) -> IntCh(Map.add var v env, bind var v s1, bind var v s2)
    | ExtCh(env, s1, s2) -> ExtCh(Map.add var v env, bind var v s1, bind var v s2)
    | Seq(env, s1, s2) -> Seq(Map.add var v env, bind var v s1, bind var v s2)
    | If(env, e, s1, s2) -> If(Map.add var v env, e, bind var v s1, bind var v s2)
    | Match(env, e, sm, ds) ->
        Match(
            Map.add var v env,
            e,
            Map.map (fun _ (var', s) -> (var', bind var v s)) sm,
            Option.map (fun (var', s) -> (var', bind var v s)) ds
        )
    | InterfaceParallel(env, s1, evs, s2) -> InterfaceParallel(Map.add var v env, bind var v s1, evs, bind var v s2)
    | Hide(env, s, evs) -> Hide(Map.add var v env, bind var v s, evs)
    | Omega env -> Omega(Map.add var v env)
    | Error _ -> s

let rec ofProc
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (env: Map<'Var, Val<'Ctor>>)
    (p: Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    : State<'P, 'Ev, 'Ch, 'Var, 'Ctor> =
    match p with
    | Proc.Unwind(n, e) -> Unwind(env, n, e)
    | Proc.Stop -> Stop(env)
    | Proc.Skip -> Skip(env)
    | Proc.Prefix(ev, p') -> Prefix(env, ev, ofProc m env p')
    | Proc.PrefixSend(ch, e, p') -> PrefixSend(env, ch, e, ofProc m env p')
    | Proc.PrefixRecv(ch, v, p') -> PrefixRecv(env, ch, v, ofProc m env p')
    | Proc.IntCh(p1, p2) -> IntCh(env, ofProc m env p1, ofProc m env p2)
    | Proc.ExtCh(p1, p2) -> ExtCh(env, ofProc m env p1, ofProc m env p2)
    | Proc.Seq(p1, p2) -> Seq(env, ofProc m env p1, ofProc m env p2)
    | Proc.If(e, p1, p2) -> If(env, e, ofProc m env p1, ofProc m env p2)
    | Proc.Match(e, pm, dp) ->
        Match(
            env,
            e,
            Map.map (fun _ (v, p) -> (v, ofProc m env p)) pm,
            Option.map (fun (v, p) -> (v, ofProc m env p)) dp
        )
    | Proc.InterfaceParallel(p1, evs, p2) -> InterfaceParallel(env, ofProc m env p1, evs, ofProc m env p2)
    | Proc.Hide(p, evs) -> Hide(env, ofProc m env p, evs)
    | Proc.Guard(e, p) -> If(env, e, ofProc m env p, Stop env)


let init
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (env: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : State<'P, 'Ev, 'Ch, 'Var, 'Ctor> =
    ofProc m env (Proc.Unwind(n, Option.map ofVal vOpt))

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
                | (Some var, p), Some e -> loop (ofProc m (Map.add var (eval env e) env) p) (Set.add s visited)
                | (None, p), None -> loop (ofProc m env p) (Set.add s visited)
                | (None, _), Some _ -> Error("given a value to Unwind, but not needed at unwind", s)
                | (Some _, _), None -> Error("needed a value by Unwind, but not given at unwind", s)
        | _ -> s

    loop s0 Set.empty

let rec trans
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (s0: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    : (Event<'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) list =
    match unwind m s0 with
    | Unwind _ -> failwith "unwind cannot return Unwind"
    | Stop _ -> []
    | Prefix(_, ev, s) -> [ (Vis ev, s) ]
    | PrefixSend(env, ch, v, s) -> [ (VisChan(ch, eval env v), s) ]
    | PrefixRecv(_, ch, v, s) -> [ (VisRecv(ch, v), s) ]
    | IntCh(_, s1, s2) -> [ (Tau, s1); (Tau, s2) ]
    | ExtCh(env, s1, s2) ->
        let t1 = trans m s1 in
        let t2 = trans m s2 in

        (List.filter (fun (ev, _) -> ev <> Tau) t1)
        @ (List.filter (fun (ev, _) -> ev <> Tau) t2)
        @ (List.fold
            (fun acc (ev, s1') ->
                match ev with
                | Tau -> (Tau, ExtCh(env, s1', s2)) :: acc
                | Event.Error -> (Event.Error, Error("lhs error in ExtCh", s0)) :: acc
                | _ -> acc)
            []
            t1)
        @ (List.fold
            (fun acc (ev, s2') ->
                match ev with
                | Tau -> (Tau, ExtCh(env, s1, s2')) :: acc
                | Event.Error -> (Event.Error, Error("rhs error in ExtCh", s0)) :: acc
                | _ -> acc)
            []
            t2)
    | Skip env -> [ (Tick, Omega env) ]
    | Seq(env, s1, s2) ->
        let t1 = trans m s1 in

        List.fold
            (fun acc (ev, s1') ->
                match ev with
                | Tick ->
                    match s1' with
                    | Omega _ -> (Tau, s2) :: acc
                    | _ -> acc // can happen?
                | Event.Error -> (Event.Error, Error("lhs error in Seq", s0)) :: acc
                | _ -> (ev, Seq(env, s1', s2)) :: acc)
            []
            t1
    | If(env, e, s1, s2) ->
        match eval env e with
        | VBool true -> trans m s1
        | VBool false -> trans m s2
        | v -> [ (Event.Error, Error($"passed a not boolean value to If: eval env {e} = {v} (env = {env})", s0)) ]
    | Match(env, e, sm, ds) ->
        match eval env e with
        | VUnion(c, v) ->
            match Map.tryFind c sm with
            | Some(var, p2) -> trans m (bind var v p2)
            | None ->
                match ds with
                | Some(var, p2) -> trans m (bind var (VUnion(c, v)) p2)
                | None -> [ (Event.Error, Error($"Match are not exhausted: | {c} {v} -> ... ", s0)) ]
        | v -> [ (Event.Error, Error($"passed a not union value to Match: eval env {e} = {v} (env = {env})", s0)) ]
    | InterfaceParallel(env, Omega _, _, Omega _) -> [ (Tick, Omega env) ] // Para6
    | InterfaceParallel(env, p1, evs, p2) ->
        let t1 = trans m p1 in
        let t2 = trans m p2 in

        (List.fold
            (fun acc (ev, p1') ->
                match ev with
                | Vis ev' ->
                    if Set.contains (Event ev') evs then
                        acc
                    else
                        (Vis ev', InterfaceParallel(env, p1', evs, p2)) :: acc // Para1
                | VisChan(ch', v) ->
                    if Set.contains (Chan ch') evs then
                        acc
                    else
                        (VisChan(ch', v), InterfaceParallel(env, p1', evs, p2)) :: acc // Para1
                | VisRecv(ch', v) ->
                    if Set.contains (Chan ch') evs then
                        acc
                    else
                        (VisRecv(ch', v), InterfaceParallel(env, p1', evs, p2)) :: acc // Para1
                | Tick ->
                    match p1' with
                    | Omega _ -> (Tau, InterfaceParallel(env, p1', evs, p2)) :: acc // Para4
                    | _ -> acc
                | Tau -> (Tau, InterfaceParallel(env, p1', evs, p2)) :: acc // Para1
                | Hid ev' -> (Hid ev', InterfaceParallel(env, p1', evs, p2)) :: acc // para1
                | HidChan(ch', v) -> (HidChan(ch', v), InterfaceParallel(env, p1', evs, p2)) :: acc // Para1
                | HidRecv(ch', v) -> (HidRecv(ch', v), InterfaceParallel(env, p1', evs, p2)) :: acc // Para1
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
                        (Vis ev', InterfaceParallel(env, p1, evs, p2')) :: acc // Para2
                | VisChan(ch', v) ->
                    if Set.contains (Chan ch') evs then
                        acc
                    else
                        (VisChan(ch', v), InterfaceParallel(env, p1, evs, p2')) :: acc
                | VisRecv(ch', v) ->
                    if Set.contains (Chan ch') evs then
                        acc
                    else
                        (VisRecv(ch', v), InterfaceParallel(env, p1, evs, p2')) :: acc // Para1
                | Tick ->
                    match p2' with
                    | Omega _ -> (Tau, InterfaceParallel(env, p1, evs, p2')) :: acc // Para5
                    | _ -> acc
                | Tau -> (Tau, InterfaceParallel(env, p1, evs, p2')) :: acc // Para2
                | Hid ev' -> (Hid ev', InterfaceParallel(env, p1, evs, p2')) :: acc // para1
                | HidChan(ch', v) -> (HidChan(ch', v), InterfaceParallel(env, p1, evs, p2')) :: acc // Para1
                | HidRecv(ch', v) -> (HidRecv(ch', v), InterfaceParallel(env, p1, evs, p2')) :: acc // Para1
                | Event.Error -> (Event.Error, Error("rhs error", s0)) :: acc)
            []
            t2)
        @ (List.fold
            (fun acc ((ev1, s1'), (ev2, s2')) ->
                match (ev1, ev2) with
                | Vis ev1', Vis ev2' when ev1' = ev2' ->
                    if Set.contains (Event ev1') evs then
                        (ev1, InterfaceParallel(env, s1', evs, s2')) :: acc // Para3
                    else
                        acc
                | VisChan(ch1, v1), VisChan(ch2, v2) when ch1 = ch2 && (v1 = v2 || v1 = VAny || v2 = VAny) ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, if v1 = VAny then v2 else v1), InterfaceParallel(env, s1', evs, s2')) :: acc // Para3
                    else
                        acc
                | VisChan(ch1, v), VisRecv(ch2, var) when ch1 = ch2 ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, v), InterfaceParallel(env, s1', evs, bind var v s2')) :: acc // Para3
                    else
                        acc
                | VisRecv(ch1, var), VisChan(ch2, v) when ch1 = ch2 ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, v), InterfaceParallel(env, bind var v s1', evs, s2')) :: acc // Para3
                    else
                        acc
                | VisRecv(ch1, _), VisRecv(ch2, _) when ch1 = ch2 ->
                    if Set.contains (Chan ch1) evs then
                        (VisChan(ch1, VAny), InterfaceParallel(env, s1', evs, s2')) :: acc // Para3
                    else
                        acc
                | Event.Error, Event.Error -> (Event.Error, Error("both lhs and rhs errors", s0)) :: acc
                | Event.Error, _ -> (Event.Error, Error("lhs error", s0)) :: acc
                | _, Event.Error -> (Event.Error, Error("rhs error", s0)) :: acc
                | _ -> acc)
            []
            (List.allPairs t1 t2))
    | Hide(env, p, evs) ->
        List.map
            (fun (ev, p') ->
                match ev with
                | Vis e when Set.contains (Event e) evs -> (Hid e, Hide(env, p', evs))
                | VisChan(ch, v) when Set.contains (Chan ch) evs -> (HidChan(ch, v), Hide(env, p', evs))
                | VisRecv(ch, v) when Set.contains (Chan ch) evs -> (HidRecv(ch, v), Hide(env, p', evs))
                | Tick ->
                    match p' with
                    | Omega _ -> (Tick, p')
                    | _ -> (ev, Hide(env, p', evs))
                | Event.Error -> (Event.Error, Error("error in Hide", s0))
                | _ -> (ev, Hide(env, p', evs)))
            (trans m p)
    | Omega _ -> []
    | Error _ -> []

let bfs
    (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>)
    (env: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    (f:
        State<'P, 'Ev, 'Ch, 'Var, 'Ctor>
            -> (Event<'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) list
            -> State<'P, 'Ev, 'Ch, 'Var, 'Ctor> list
            -> Unit)
    =
    let mutable q: Queue<State<'P, 'Ev, 'Ch, 'Var, 'Ctor> list> =
        Queue.conj [ init m env n vOpt ] Queue.empty in

    while not (Queue.isEmpty q) do
        let path, q' = Queue.uncons q in
        q <- q'
        let s = List.head path in
        let t = trans m s in
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
    (env: Map<'Var, Val<'Ctor>>)
    (n: 'P)
    (vOpt: Val<'Ctor> option)
    : (State<'P, 'Ev, 'Ch, 'Var, 'Ctor> * Event<'Ev, 'Ch, 'Var, 'Ctor> * State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) list =
    let rec loop s visited =
        List.fold
            (fun (accr, accs) (s1, ev, s1') ->
                if Set.contains (unwind m s1') accs then
                    ((s1, ev, s1') :: accr, accs)
                else
                    let accr', accs' = loop s1' (Set.add (unwind m s1) accs)
                    ((s1, ev, s1') :: accr @ accr', accs'))
            ([], Set.add (unwind m s) visited)
            (List.map (fun (ev, s') -> (s, ev, s')) (trans m s))

    fst (loop (Unwind(env, n, Option.map ofVal vOpt)) Set.empty)

let format (m: ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor>) (s0: State<'P, 'Ev, 'Ch, 'Var, 'Ctor>) : string =
    let rec f s isTop =
        match s with
        | Unwind(env, n, eOpt) ->
            if isTop then
                f (unwind m s) false
            else
                match eOpt with
                | Some e -> $"({n} {Expr.format e}{Env.format env})"
                | None -> $"({n}{Env.format env})"
        | Stop env -> $"(STOP{Env.format env})"
        | Skip env -> $"(SKIP{Env.format env})"
        | Prefix(env, ev, s') -> $"({ev} -> {f s' false}{Env.format env})"
        | PrefixSend(env, ev, v, s') -> $"({ev}!{v} -> {f s' false}{Env.format env})"
        | PrefixRecv(env, ev, v, s') -> $"({ev}?{v} -> {f s' false}{Env.format env})"
        | IntCh(env, s1, s2) -> $"({f s1 false} ⨅ {f s2 false}{Env.format env})"
        | ExtCh(env, s1, s2) -> $"({f s1 false} □ {f s2 false}{Env.format env})"
        | Seq(env, s1, s2) -> $"({f s1 false} ; {f s2 false}{Env.format env})"
        | If(env, e, s1, s2) -> $"(if {Expr.format e} then {f s1 false} else {f s2 false}{Env.format env})"
        | Match(env, e, sm, ds) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, p')) -> $"{c} {v} -> {f p' false}") (Map.toList sm)

            match ds with
            | Some(v, p') ->
                $"(match {Expr.format e} with {String.concat sep cs'} | {v} -> {f p' false}{Env.format env})"
            | None -> $"(match {Expr.format e} with {String.concat sep cs'}{Env.format env})"
        | InterfaceParallel(env, s1, evs, s2) -> $"({f s1 false} ⟦{EventSpec.format evs}⟧ {f s2 false}{Env.format env})"
        | Hide(env, s, evs) -> $"({f s false} \\\\ {EventSpec.format evs}{Env.format env})"
        | Omega env -> $"(Ω{Env.format env})"
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
