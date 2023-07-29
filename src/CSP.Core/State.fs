module CSP.Core.State

open CSP.Core.UnwindError
open FSharpx.Collections
open CSP.Core.Eval
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
    | Unwind of Env * ProcId * Expr<unit> list
    | Stop
    | Skip
    | Prefix of Env * Expr<unit> * State
    | PrefixRecv of Env * Expr<unit> * Var * State
    | IntCh of State * State
    | ExtCh of State * State
    | Seq of State * State
    | If of Env * Expr<unit> * State * State
    | Match of Env * Expr<unit> * Map<Ctor option, Var option list * State>
    | InterfaceParallel of Env * State * Expr<unit> * State
    | Hide of Env * State * Expr<unit>
    | Omega
    | ErrorState of string

let rec bind1 (var: Var) (v: Val) (s: State) : Result<State, EnvError> =
    match s with
    | Unwind(env, n, eOpt) -> Result.map (fun env -> Unwind(env, n, eOpt)) (Env.bind1 var v env)
    | Stop _ -> Ok(Stop)
    | Skip _ -> Ok(Skip)
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
    | Match(env, e, sm) ->
        let smRes =
            Map.fold
                (fun smAccRes ctor (vars, s) ->
                    Result.bind
                        (fun smAcc -> Result.map (fun s -> Map.add ctor (vars, s) smAcc) (bind1 var v s))
                        smAccRes)
                (Ok Map.empty)
                sm in

        match Env.bind1 var v env, smRes with
        | Ok(env), Ok(sm) -> Ok(Match(env, e, sm))
        | Error(err), _ -> Error err
        | _, Error(err) -> Error err
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
    | ErrorState(msg) -> Ok(ErrorState(msg))

let bindAll (xs: (Var option * Val) seq) (s: State) =
    Seq.fold
        (fun sRes (varOpt, v) ->
            match varOpt with
            | Some var -> Result.bind (bind1 var v) sRes
            | None -> sRes)
        (Ok(s))
        xs

let ofProc (genv: Env) (p: Proc<unit>) : State =
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
        | Proc.Match(e, mp, _) -> Match(genv, e, Map.map (fun _ (varOpts, p) -> (varOpts, ofProc p)) mp)
        | Proc.InterfaceParallel(p1, expr, p2, _) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            InterfaceParallel(genv, s1, expr, s2)
        | Proc.Interleave(p1, p2, _) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            InterfaceParallel(genv, s1, LitEmpty(TSet(TTuple([], __LINE__), __LINE__), (), __LINE__), s2)
        | Proc.Hide(p, expr, _) -> let s = ofProc p in Hide(genv, s, expr)
        | Proc.Guard(e, p, _) -> let s = ofProc p in If(genv, e, s, Stop)

    ofProc p

type UnwindConfig = { EvalConfig: EvalConfig }
let unwindConfig cfg = { EvalConfig = cfg }

let unwind (cfg: UnwindConfig) (pm: ProcMap<unit>) (cm: CtorMap) (genv: Env) (s0: State) : Result<State, UnwindError> =
    let rec unwind s visited =
        match s with
        | Unwind(env, pn, exprs) ->
            if Set.contains pn visited then
                Error(Recursion(pn))
            else
                match tryFind pn pm with
                | Some(varOpts, p) ->
                    let vsRes =
                        List.foldBack
                            (fun expr vsRes ->
                                Result.bind
                                    (fun vs -> Result.map (fun v -> v :: vs) (eval cfg.EvalConfig cm env expr))
                                    vsRes)
                            exprs
                            (Ok([]))

                    match vsRes with
                    | Ok vs ->
                        if List.length varOpts = List.length vs then
                            match Env.bindAll (List.zip varOpts vs) genv with
                            | Ok env -> unwind (ofProc env p) (Set.add pn visited)
                            | Error err -> Error(EnvError err)
                        else
                            Error(ArgumentsLengthMismatch(List.length exprs, varOpts))
                    | Error err -> Error(EvalError err)
                | None -> Error(NoSuchProcess(pn, procIds pm))
        | _ -> Ok(s)

    unwind s0 Set.empty

let format (genv: Env) (s0: State) : string =
    let formatExpr = Expr.format noAnnotation

    let rec f s =
        match s with
        | Unwind(env, n, exprs) ->
            if List.isEmpty exprs then
                $"%s{n}"
            else
                let s = String.concat "" (List.map (fun expr -> $" %s{formatExpr expr}") exprs) in
                $"%s{n}%s{s} env=%s{Env.format genv env}"
        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(env, expr, s') -> $"(%s{formatExpr expr} -> %s{f s'} env=%s{Env.format genv env})"
        | PrefixRecv(env, expr, var, s') -> $"({formatExpr expr}?{var} -> {f s'} env={Env.format genv env})"
        | IntCh(s1, s2) -> $"(%s{f s1} ⨅ %s{f s2})"
        | ExtCh(s1, s2) -> $"(%s{f s1} □ %s{f s2})"
        | Seq(s1, s2) -> $"(%s{f s1} ; %s{f s2})"
        | If(env, expr, s1, s2) ->
            $"(if %s{formatExpr expr} then %s{f s1} else %s{f s2}) env=%s{Env.format genv env})"
        | Match(env, expr, sm) ->
            let sep = " | " in

            let cs' =
                List.map
                    (fun (ctorOpt, (varOpts, p')) ->
                        let s1 =
                            match ctorOpt with
                            | Some ctor -> Ctor.format ctor
                            | None -> "_" in

                        let s2 =
                            if List.isEmpty varOpts then
                                ""
                            else
                                String.concat
                                    ""
                                    (List.map
                                        (fun varOpt ->
                                            match varOpt with
                                            | Some var -> $" %s{Var.format var}"
                                            | None -> " _")
                                        varOpts) in

                        $"%s{s1}%s{s2} -> %s{f p'}")
                    (Map.toList sm) in

            $"(match %s{formatExpr expr} with %s{String.concat sep cs'} env=%s{Env.format genv env})"
        | InterfaceParallel(env, s1, expr, s2) ->
            $"(%s{f s1} ⟦%s{formatExpr expr}⟧ %s{f s2} env=%s{Env.format genv env})"
        | Hide(env, s, expr) -> $"(%s{f s} \\\\ %s{formatExpr expr} env=%s{Env.format genv env})"
        | Omega -> "Ω"
        | ErrorState(msg) -> $"(ERROR: %s{msg})"

    f s0
