module CSP.Core.Proc

open CSP.Core.Val
open CSP.Core.Expr
open FSharpx.Collections

type Proc<'P, 'E, 'V, 'C when 'E: comparison and 'V: comparison and 'C: comparison> =
    | Unwind of 'P
    | Stop
    | Skip
    | Prefix of 'E * Proc<'P, 'E, 'V, 'C>
    | IntCh of Proc<'P, 'E, 'V, 'C> * Proc<'P, 'E, 'V, 'C>
    | ExtCh of Proc<'P, 'E, 'V, 'C> * Proc<'P, 'E, 'V, 'C>
    | Seq of Proc<'P, 'E, 'V, 'C> * Proc<'P, 'E, 'V, 'C>
    | IfProc of Expr<'P, 'E, 'V, 'C> * Proc<'P, 'E, 'V, 'C> * Proc<'P, 'E, 'V, 'C>
    | MatchProc of Expr<'P, 'E, 'V, 'C> * Map<Ctor<'C>, 'V * Proc<'P, 'E, 'V, 'C>> * ('V * Proc<'P, 'E, 'V, 'C>) option
    | InterfaceParallel of Proc<'P, 'E, 'V, 'C> * Set<'E> * Proc<'P, 'E, 'V, 'C>
    | Hide of Proc<'P, 'E, 'V, 'C> * Set<'E>
    | Omega

let unwind m p =
    match p with
    | Unwind n -> Map.find n m
    | _ -> p

let format (m: Map<'P, Proc<'P, 'E, 'V, 'C>>) (p0: Proc<'P, 'E, 'V, 'C>) : string =
    let rec loop p =
        match p with
        | Unwind n -> $"{n}"
        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(ev, p') -> $"({ev} -> {loop p'})"
        | IntCh(p1, p2) -> $"({loop p1} ⨅ {loop p2})"
        | ExtCh(p1, p2) -> $"({loop p1} □ {loop p2})"
        | Seq(p1, p2) -> $"({loop p1} ; {loop p2})"
        | IfProc(e, p1, p2) -> $"(if {format e} then {loop p1} else {loop p2})"
        | MatchProc(e, cs, d) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, e')) -> $"{c} {v} -> {loop e'}") (Map.toList cs)

            match d with
            | Some(v, e') -> $"(match {format e} with {String.concat sep cs'} | {v} -> {loop e'})"
            | None -> $"(match {format e} with {String.concat sep cs'})"
        | InterfaceParallel(e1, evs, e2) ->
            let sep = ", "
            let es' = List.map (fun ev -> $"{ev}") (Set.toList evs) in
            $"({loop e1} ⟦{{{String.concat sep es'}}}⟧ {loop e2})"
        | Hide(p, evs) ->
            let evs' = List.map (fun ev -> $"{ev}") (Set.toList evs) in
            let sep = ", "
            $"({loop p} \\\\ {String.concat sep evs'})"
        | Omega -> "Ω" in

    match p0 with
    | Unwind n -> loop (Map.find n m)
    | Stop -> "STOP"
    | Skip -> "SKIP"
    | Prefix(ev, p') -> $"({ev} -> {loop p'})"
    | IntCh(p1, p2) -> $"({loop p1} ⨅ {loop p2})"
    | ExtCh(p1, p2) -> $"({loop p1} □ {loop p2})"
    | Seq(p1, p2) -> $"({loop p1} ; {loop p2})"
    | IfProc(e, p1, p2) -> $"(if {format e} then {loop p1} else {loop p2})"
    | MatchProc(e, cs, d) ->
        let sep = " | " in
        let cs' = List.map (fun (c, (v, e')) -> $"{c} {v} -> {loop e'}") (Map.toList cs)

        match d with
        | Some(v, e') -> $"(match {format e} with {String.concat sep cs'} | {v} -> {loop e'})"
        | None -> $"(match {format e} with {String.concat sep cs'})"
    | InterfaceParallel(e1, evs, e2) ->
        let sep = ", "
        let es' = List.map (fun ev -> $"{ev}") (Set.toList evs) in
        $"({loop e1} ⟦{{{String.concat sep es'}}}⟧ {loop e2})"
    | Hide(p, evs) ->
        let evs' = List.map (fun ev -> $"{ev}") (Set.toList evs) in
        let sep = ", "
        $"({loop p} \\\\ {String.concat sep evs'})"
    | Omega -> "Ω"

type Event<'E> =
    | Vis of 'E
    | Tau
    | Tick

let rec trans
    (m: Map<'P, Proc<'P, 'E, 'V, 'C>>)
    (env: Map<'V, Val<'C>>)
    (p0: Proc<'P, 'E, 'V, 'C>)
    : (Event<'E> * Proc<'P, 'E, 'V, 'C>) list =
    match p0 with
    | Unwind n -> trans m env (Map.find n m)
    | Stop -> []
    | Prefix(ev, p1) -> [ (Vis ev, p1) ]
    | IntCh(p1, p2) -> [ (Tau, p1); (Tau, p2) ]
    | ExtCh(p1, p2) ->
        let t1 = trans m env p1 in
        let t2 = trans m env p2 in

        (List.filter (fun (ev, _) -> ev <> Tau) t1)
        @ (List.filter (fun (ev, _) -> ev <> Tau) t2)
        @ (List.fold (fun acc (ev, p1') -> if ev = Tau then (Tau, ExtCh(p1', p2)) :: acc else acc) [] t1)
        @ (List.fold (fun acc (ev, p2') -> if ev = Tau then (Tau, ExtCh(p1, p2')) :: acc else acc) [] t2)
    | Skip -> [ (Tick, Omega) ]
    | Seq(p1, p2) ->
        let t1 = trans m env p1 in

        List.map (fun (ev, p1') -> (ev, Seq(p1', p2))) (List.filter (fun (ev, _) -> ev <> Tick) t1)
        @ (if List.exists (fun (ev, p1') -> ev = Tick && p1' = Omega) t1 then
               [ (Tau, p2) ]
           else
               [])
    | IfProc(expr, p1, p2) ->
        match eval env expr with
        | ValBool true -> trans m env p1
        | ValBool false -> trans m env p2
        | x -> failwith $"expression did not return a boolean value %A{x}"
    | MatchProc(e0, pm, p1) ->
        match eval env e0 with
        | ValUnion(c, v) ->
            match Map.tryFind c pm with
            | Some(x, p2) -> trans m (Map.add x v env) p2
            | None ->
                match p1 with
                | Some(x, p2) -> trans m (Map.add x (ValUnion(c, v)) env) p2
                | None -> []
        | x -> failwith $"expression did not return an union value %A{x}"
    | InterfaceParallel(Omega, _, Omega) -> [ (Tick, Omega) ] // Para6
    | InterfaceParallel(p1, evs, p2) ->
        let t1 = trans m env p1 in
        let t2 = trans m env p2 in

        (List.fold
            (fun acc (ev, p1') ->
                match ev with
                | Vis ev' ->
                    if Set.contains ev' evs then
                        acc
                    else
                        (Vis ev', InterfaceParallel(p1', evs, p2)) :: acc // Para1
                | Tick ->
                    if p1' = Omega then
                        (Tau, InterfaceParallel(Omega, evs, p2)) :: acc // Para4
                    else
                        acc
                | Tau -> (Tau, InterfaceParallel(p1', evs, p2)) :: acc) // Para1
            []
            t1)
        @ (List.fold
            (fun acc (ev, p2') ->
                match ev with
                | Vis ev' ->
                    if Set.contains ev' evs then
                        acc
                    else
                        (Vis ev', InterfaceParallel(p1, evs, p2')) :: acc // Para2
                | Tick ->
                    if p2' = Omega then
                        (Tau, InterfaceParallel(p1, evs, Omega)) :: acc // Para5
                    else
                        acc
                | Tau -> (Tau, InterfaceParallel(p1, evs, p2')) :: acc) // Para2
            []
            t2)
        @ (List.fold
            (fun acc ((ev1, p1'), (ev2, p2')) ->
                if ev1 = ev2 then
                    match ev1 with
                    | Vis ev' ->
                        if Set.contains ev' evs then
                            (ev1, InterfaceParallel(p1', evs, p2')) :: acc // Para3
                        else
                            acc
                    | Tick -> acc
                    | Tau -> acc
                else
                    acc)
            []
            (List.allPairs t1 t2))
    | Hide(p, evs) ->
        List.map
            (fun (ev, p') ->
                match ev with
                | Vis e when Set.contains e evs -> (Tau, Hide(p', evs))
                | Tick when p' = Omega -> (Tick, Omega)
                | _ -> (ev, Hide(p', evs)))
            (trans m env p)
    | Omega -> []

let rec traces
    (m: Map<'P, Proc<'P, 'E, 'V, 'C>>)
    (env: Map<'V, Val<'C>>)
    (p: Proc<'P, 'E, 'V, 'C>)
    (depth: int)
    : Event<'E> list list =
    if depth < 1 then
        []
    else if depth = 1 then
        List.map (fun (ev, _) -> [ ev ]) (trans m env p)
    else
        let t = trans m env p in

        List.map (fun (ev, _) -> [ ev ]) t
        @ (List.collect (fun (ev, p') -> List.map (fun evs -> ev :: evs) (traces m env p' (depth - 1))) t)

let bfs
    (m: Map<'P, Proc<'P, 'E, 'V, 'C>>)
    (env: Map<'V, Val<'C>>)
    (n: 'P)
    (f: Proc<'P, 'E, 'V, 'C> -> (Event<'E> * Proc<'P, 'E, 'V, 'C>) list -> Proc<'P, 'E, 'V, 'C> list -> Unit)
    =
    let mutable q: Queue<Proc<'P, 'E, 'V, 'C> list> =
        Queue.conj [ Unwind n ] Queue.empty in

    while not (Queue.isEmpty q) do
        let path, q' = Queue.uncons q in
        q <- q'
        let p = List.head path in
        let t = trans m env p in
        f p t (List.rev path)

        List.iter
            (fun p' ->
                let u =
                    match p' with
                    | Unwind n -> Map.find n m
                    | _ -> p in

                if not (List.contains u path) then
                    q <- Queue.conj (u :: path) q)
            (List.map (fun (_, p') -> p') t)

let deadlocks (m: Map<'P, Proc<'P, 'E, 'V, 'C>>) (env: Map<'V, Val<'C>>) (n: 'P) : Proc<'P, 'E, 'V, 'C> list list =
    let mutable r: Proc<'P, 'E, 'V, 'C> list list = [] in

    bfs m env n (fun _ t path ->
        if List.isEmpty t then
            r <- path :: r)

    r

let edges
    (m: Map<'P, Proc<'P, 'E, 'V, 'C>>)
    (env: Map<'V, Val<'C>>)
    (n: 'P)
    : (Proc<'P, 'E, 'V, 'C> * Event<'E> * Proc<'P, 'E, 'V, 'C>) list =
    let rec loop p s =
        List.fold
            (fun (accr, accs) (p1, ev, p1') ->
                if Set.contains (unwind m p1') accs then
                    ((p1, ev, p1') :: accr, accs)
                else
                    let accr', accs' = loop p1' (Set.add (unwind m p1) accs)
                    ((p1, ev, p1') :: accr @ accr', accs'))
            ([], Set.add (unwind m p) s)
            (List.map (fun (ev, p') -> (p, ev, p')) (trans m env p))

    fst (loop (Unwind n) Set.empty)


let dot (m: Map<'P, Proc<'P, 'E, 'V, 'C>>) (env: Map<'V, Val<'C>>) (n: 'P) : string =
    let dq = "\""
    let sq = "'"

    let r' =
        List.map
            (fun (p, ev, p') ->
                let ev' = ev.ToString()
                $"  \"{(format m p).Replace(dq, sq)}\" -> \"{(format m p').Replace(dq, sq)}\" [label=\"{ev'.Replace(dq, sq)}\"]")
            (edges m env n)

    let sep = "\n"

    $"""digraph G {{
{String.concat sep r'}
}}"""
