module CSP.Core.State

open CSP.Core.Eval
open FSharpx.Collections
open CSP.Core.Env
open CSP.Core.EnvError
open CSP.Core.CtorMap
open CSP.Core.Type
open CSP.Core.Var
open CSP.Core.Ctor
open CSP.Core.Expr
open CSP.Core.ProcMap
open CSP.Core.Proc
open CSP.Core.Val

type State =
    | Unwind of Env * ProcId * Expr option
    | Stop
    | Skip
    | Prefix of Env * Expr * State
    | PrefixRecv of Env * Expr * Var * State
    | IntCh of State * State
    | ExtCh of State * State
    | Seq of State * State
    | If of Env * Expr * State * State
    | Match of Env * Expr * Map<Ctor, Var list * State> * (Var option * State) option
    | InterfaceParallel of Env * State * Expr * State
    | Hide of Env * State * Expr
    | Omega
    | ErrorState of string * State

let rec bind1 (var: Var) (v: Val) (s: State) : Result<State, EnvError> =
    match s with
    | Unwind(env, n, eOpt) -> Result.map (fun env -> Unwind(env, n, eOpt)) (Env.bind1 var v env)
    | Stop -> Ok(Stop)
    | Skip -> Ok(Skip)
    | Prefix(env, expr, s') ->
        match Env.bind1 var v env, bind1 var v s' with
        | Ok(env), Ok(s') -> Ok(Prefix(env, expr, s'))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | PrefixRecv(env, expr, v', s') ->
        match Env.bind1 var v env, bind1 var v s' with
        | Ok(env), Ok(s') -> Ok(PrefixRecv(env, expr, v', s'))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | IntCh(s1, s2) ->
        match bind1 var v s1, bind1 var v s2 with
        | Ok(p1), Ok(p2) -> Ok(IntCh(p1, p2))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | ExtCh(s1, s2) ->
        match bind1 var v s1, bind1 var v s2 with
        | Ok(p1), Ok(p2) -> Ok(ExtCh(p1, p2))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | Seq(s1, s2) ->
        match bind1 var v s1, bind1 var v s2 with
        | Ok(p1), Ok(p2) -> Ok(Seq(p1, p2))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | If(env, e, s1, s2) ->
        match Env.bind1 var v env, bind1 var v s1, bind1 var v s2 with
        | Ok(env), Ok(s1), Ok(s2) -> Ok(If(env, e, s1, s2))
        | Error err, _, _ -> Error(err)
        | _, Error err, _ -> Error(err)
        | _, _, Error err -> Error(err)
    | Match(env, e, sm, ds) ->
        let smRes =
            Map.fold
                (fun smAccRes ctor (vars, s) ->
                    Result.bind
                        (fun smAcc -> Result.map (fun s -> Map.add ctor (vars, s) smAcc) (bind1 var v s))
                        smAccRes)
                (Ok Map.empty)
                sm in

        let dsRes =
            match ds with
            | Some(varOpt, s) ->
                match bind1 var v s with
                | Ok(s) -> Ok(Some(varOpt, s))
                | Error err -> Error err
            | None -> Ok(None) in

        match Env.bind1 var v env, smRes, dsRes with
        | Ok(env), Ok(sm), Ok(ds) -> Ok(Match(env, e, sm, ds))
        | Error(err), _, _ -> Error err
        | _, Error(err), _ -> Error err
        | _, _, Error(err) -> Error err
    | InterfaceParallel(env, s1, expr, s2) ->
        match Env.bind1 var v env, bind1 var v s1, bind1 var v s2 with
        | Ok(env), Ok(s1), Ok(s2) -> Ok(InterfaceParallel(env, s1, expr, s2))
        | Error(err), _, _ -> Error err
        | _, Error(err), _ -> Error err
        | _, _, Error(err) -> Error err
    | Hide(env, s, expr) ->
        match Env.bind1 var v env, bind1 var v s with
        | Ok(env), Ok(s) -> Ok(Hide(env, s, expr))
        | Error(err), _ -> Error err
        | _, Error(err) -> Error err
    | Omega -> Ok(Omega)
    | ErrorState(msg, s) -> Ok(ErrorState(msg, s))

let bindAll (xs: (Var * Val) seq) (s: State) =
    Seq.fold (fun sRes (var, v) -> Result.bind (bind1 var v) sRes) (Ok(s)) xs

let ofProc (genv: Env) (p: Proc) : State =
    let rec ofProc p =
        match p with
        | Proc.Unwind(nm, e, _) -> Unwind(genv, nm, e)
        | Proc.Stop _ -> Stop
        | Proc.Skip _ -> Skip
        | Proc.Prefix(e, p', _) -> Prefix(genv, e, ofProc p')
        | Proc.PrefixRecv(e, var, p', _) -> PrefixRecv(genv, e, var, ofProc p')
        | Proc.IntCh(p1, p2, _) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            IntCh(s1, s2)
        | Proc.ExtCh(p1, p2, _) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            ExtCh(s1, s2)
        | Proc.Seq(p1, p2, _) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            Seq(s1, s2)
        | Proc.If(e, p1, p2, _) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            If(genv, e, s1, s2)
        | Proc.Match(e, mp, dp, _) ->
            let ms = Map.map (fun _ (var, p) -> (var, ofProc p)) mp in
            let ds = Option.map (fun (var, p) -> (var, ofProc p)) dp

            Match(genv, e, ms, ds)
        | Proc.InterfaceParallel(p1, expr, p2, _) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            InterfaceParallel(genv, s1, expr, s2)
        | Proc.Interleave(p1, p2, line) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            InterfaceParallel(genv, s1, LitEmpty(TSet(TTuple([])), None, line), s2)
        | Proc.Hide(p, expr, _) -> let s = ofProc p in Hide(genv, s, expr)
        | Proc.Guard(e, p, _) -> let s = ofProc p in If(genv, e, s, Stop)

    ofProc p

type UnwindConfig = { EvalConfig: EvalConfig }
let unwindConfig cfg = { EvalConfig = cfg }

let unwind (cfg: UnwindConfig) (pm: ProcMap) (cm: CtorMap) (genv: Env) (s0: State) : State =
    let rec loop s visited =
        match s with
        | Unwind(env, pn, eOpt) ->
            if Set.contains s visited then
                failwith "circular definition"
            else
                match Map.tryFind pn pm with
                | Some t ->
                    match t, eOpt with
                    | (Some var, p), Some e ->
                        match eval cfg.EvalConfig cm env e with
                        | Ok v ->
                            match Env.bind1 var v genv with
                            | Ok env -> loop (ofProc env p) (Set.add (ofProc env p) visited)
                            | Error err -> ErrorState(EnvError.format err, s)
                        | Error err -> ErrorState(EvalError.format err, s)
                    | (None, p), None -> loop (ofProc env p) (Set.add (ofProc env p) visited)
                    | (None, _), Some _ -> ErrorState("given a value to Unwind, but not needed at unwind", s)
                    | (Some _, _), None -> ErrorState("needed a value by Unwind, but not given at unwind", s)
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
                                (Map.toList pm)) in

                    failwith $"no such process: {pn} in [{ms}]"
        | _ -> s

    loop s0 Set.empty

let format (cfg: UnwindConfig) (pm: ProcMap) (cm: CtorMap) (genv: Env) (s0: State) : string =
    let rec f s isTop =
        match s with
        | Unwind(env, n, eOpt) ->
            if isTop then
                // TODO: Do not use unwind
                f (unwind cfg pm cm genv s) false
            else
                match eOpt with
                | Some e -> $"%s{n} %s{Expr.format e} env=%s{Env.format genv env}"
                | None -> $"%s{n}"
        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(env, expr, s') -> $"(%s{Expr.format expr} -> %s{f s' false} env=%s{Env.format genv env})"
        | PrefixRecv(env, expr, var, s') -> $"({Expr.format expr}?{var} -> {f s' false} env={Env.format genv env})"
        | IntCh(s1, s2) -> $"(%s{f s1 false} ⨅ %s{f s2 false})"
        | ExtCh(s1, s2) -> $"(%s{f s1 false} □ %s{f s2 false})"
        | Seq(s1, s2) -> $"(%s{f s1 false} ; %s{f s2 false})"
        | If(env, expr, s1, s2) ->
            $"(if %s{Expr.format expr} then %s{f s1 false} else %s{f s2 false}) env=%s{Env.format genv env})"
        | Match(env, expr, sm, ds) ->
            let sep = " | " in

            let cs' =
                List.map
                    (fun (c, (vs, p')) ->
                        let s = String.concat " " (List.map Var.format vs) in
                        $"%s{Ctor.format c} %s{s} -> %s{f p' false}")
                    (Map.toList sm) in

            match ds with
            | Some(Some var, p') ->
                $"(match %s{Expr.format expr} with %s{String.concat sep cs'} | %s{Var.format var} -> %s{f p' false} env=%s{Env.format genv env})"
            | Some(None, p') ->
                $"(match %s{Expr.format expr} with %s{String.concat sep cs'} | _ -> %s{f p' false} env=%s{Env.format genv env})"
            | None -> $"(match %s{Expr.format expr} with %s{String.concat sep cs'} env=%s{Env.format genv env})"
        | InterfaceParallel(env, s1, expr, s2) ->
            $"(%s{f s1 false} ⟦%s{Expr.format expr}⟧ %s{f s2 false} env=%s{Env.format genv env})"
        | Hide(env, s, expr) -> $"(%s{f s false} \\\\ %s{Expr.format expr} env=%s{Env.format genv env})"
        | Omega -> "Ω"
        | ErrorState(msg, s) -> $"(ERROR: %s{msg} at %s{f s false})"

    f s0 true
