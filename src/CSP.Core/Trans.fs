module CSP.Core.Trans

open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.Event
open CSP.Core.ProcEval
open CSP.Core.ProcEvalError
open CSP.Core.ProcMap
open CSP.Core.State
open CSP.Core.Util

type TransConfig = { ProcEvalConfig: ProcEvalConfig }

let trans
    (cfg: TransConfig)
    (pm: ProcMap<unit>)
    (cm: CtorMap)
    (genv: Env)
    (s: State)
    : Result<(Event * State) list, ProcEvalError> =
    let eval = eval cfg.ProcEvalConfig cm in

    let rec trans s =
        match s with
        | Unwind(pn, vs) ->
            match pm with
            | ProcMap pm ->
                match Map.tryFind pn pm with
                | None -> Error(NoSuchProcess(pn))
                | Some(varOpts, p) ->
                    if List.length varOpts = List.length vs then
                        let env = bindAllOpts (List.zip varOpts vs) genv in
                        eval env p |> Result.bind trans
                    else
                        Error(ArgumentsLengthMismatch(pn, varOpts, vs))
            
        | Skip -> Ok [ (Tick, Omega) ] // Skip
        | Prefix(v, s) -> Ok [ (Vis v, s) ] // Prefix
        | PrefixRecv(vs, env, var, p) ->
            ResultEx.bindAll
                (fun v -> let env = bind1 var v env in p |> eval env |> Result.map (fun s -> (Vis v, s)))
                (Set.toSeq vs)
        | IntCh(s1, s2) ->
            Ok
                [ (Tau, s1) // IntCh1
                  (Tau, s2) ] // IntCh2
        | ExtCh(s1, s2) ->
            let ts1 s1 =
                trans s1
                |> Result.map (
                    List.map (fun (ev, s1') ->
                        match ev with
                        | Tau -> (Tau, ExtCh(s1', s2)) // ExtCh3
                        | _ -> (ev, s1')) // ExtCh1
                )

            let ts2 s2 =
                trans s2
                |> Result.map (
                    List.map (fun (ev, s2') ->
                        match ev with
                        | Tau -> (Tau, ExtCh(s1, s2')) // ExtCh4
                        | _ -> (ev, s2')) // ExtCh2
                )

            (s1, s2) |> ResultEx.bind2 ts1 ts2 |> Result.map (fun (ts1, ts2) -> ts1 @ ts2)
        | Seq(s1, s2) ->
            trans s1
            |> Result.map (
                List.map (fun (ev, s1') ->
                    match ev with
                    | Tick -> (Tau, s2) // Seq2
                    | _ -> (ev, s1'))
            ) // Seq1
        | InterfaceParallel(Omega, _, Omega) -> Ok [ (Tick, Omega) ] // Para6
        | InterfaceParallel(s1, vs, s2) ->
            match trans s1, trans s2 with
            | Ok(ts1), Ok(ts2) ->
                let ts1' =
                    List.collect
                        (fun (ev, s1') ->
                            match ev with
                            | Vis v when not (Set.contains v vs) -> [ (ev, InterfaceParallel(s1', vs, s2)) ] // Para1
                            | Tau -> [ (Tau, InterfaceParallel(s1', vs, s2)) ] // Para1
                            | Hid v -> [ (Hid v, InterfaceParallel(s1', vs, s2)) ] // Para1
                            | Tick -> [ (Tau, InterfaceParallel(Omega, vs, s2)) ] // Para4
                            | _ -> [])
                        ts1 in

                let ts2' =
                    List.collect
                        (fun (ev, s2') ->
                            match ev with
                            | Vis v when not (Set.contains v vs) -> [ (ev, InterfaceParallel(s1, vs, s2')) ] // Para2
                            | Tau -> [ (Tau, InterfaceParallel(s1, vs, s2')) ] // Para2
                            | Hid v -> [ (Hid v, InterfaceParallel(s1, vs, s2')) ] // Para2
                            | Tick -> [ (Tau, InterfaceParallel(s1, vs, Omega)) ] // Para5
                            | _ -> [])
                        ts2 in

                let t1t2s' =
                    ListEx.cartesian2 ts1 ts2
                    |> List.collect (fun ((ev1, s1'), (ev2, s2')) ->
                        match ev1, ev2 with
                        | Vis v1, Vis v2 when v1 = v2 && Set.contains v1 vs ->
                            [ (Vis v1, InterfaceParallel(s1', vs, s2')) ] // Para3
                        | _ -> [])

                Ok(ts1' @ ts2' @ t1t2s')
            | Error(err), _ -> Error(err)
            | _, Error(err) -> Error(err)
        | Hide(s, vs) ->
            trans s
            |> Result.map (
                List.collect (fun (ev, s') ->
                    match ev with
                    | Vis(v) when not (Set.contains v vs) -> [ (Vis(v), s') ] // Hide1
                    | Vis(v) -> [ (Hid(v), s') ] // Hide2
                    | Tick -> [ (Tick, Omega) ] // Hide3
                    | _ -> [])
            )
        | _ -> Ok []

    trans s
