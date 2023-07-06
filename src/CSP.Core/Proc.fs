module CSP.Core.Proc

open CSP.Core.Val
open CSP.Core.Expr

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
    | Omega
    
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
                        (Tau, InterfaceParallel(Omega, evs, p2)) :: acc
                    else
                        acc // Para4
                | Tau -> (Tau, InterfaceParallel(p1', evs, p2)) :: acc)
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
                        (Tau, InterfaceParallel(p1, evs, Omega)) :: acc
                    else
                        acc // Para5
                | Tau -> (Tau, InterfaceParallel(p1, evs, p2')) :: acc)
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
                    | Tick ->
                        if p1' = Omega && p2' = Omega then
                            (Tick, Omega) :: acc
                        else
                            acc // Para6
                    | Tau -> (Tau, InterfaceParallel(p1, evs, p2')) :: acc
                else
                    acc)
            []
            (List.allPairs t1 t2))
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
        List.map (fun (ev, _) -> [ ev ]) t @ (List.collect (fun (ev, p') -> List.map (fun evs -> ev :: evs) (traces m env p' (depth - 1))) t)
