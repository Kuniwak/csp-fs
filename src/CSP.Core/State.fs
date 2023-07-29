module CSP.Core.State

open CSP.Core.UnwindError
open FSharpx.Collections
open CSP.Core.Eval
open CSP.Core.LineNum
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
    | Unwind of Env * ProcId * Expr<unit> list * LineNum
    | Stop of LineNum
    | Skip of LineNum
    | Prefix of Env * Expr<unit> * State * LineNum
    | PrefixRecv of Env * Expr<unit> * Var * State * LineNum
    | IntCh of State * State * LineNum
    | ExtCh of State * State * LineNum
    | Seq of State * State * LineNum
    | If of Env * Expr<unit> * State * State * LineNum
    | Match of Env * Expr<unit> * Map<Ctor option, Var option list * State> * LineNum
    | InterfaceParallel of Env * State * Expr<unit> * State * LineNum
    | Hide of Env * State * Expr<unit> * LineNum
    | Omega
    | ErrorState of string

let rec bind1 (var: Var) (v: Val) (s: State) : Result<State, EnvError> =
    match s with
    | Unwind(env, n, eOpt, line) -> Result.map (fun env -> Unwind(env, n, eOpt, line)) (Env.bind1 var v env)
    | Stop(line) -> Ok(Stop(line))
    | Skip(line) -> Ok(Skip(line))
    | Prefix(env, expr, s', line) ->
        match Env.bind1 var v env, bind1 var v s' with
        | Ok(env), Ok(s') -> Ok(Prefix(env, expr, s', line))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | PrefixRecv(env, expr, v', s', line) ->
        match Env.bind1 var v env, bind1 var v s' with
        | Ok(env), Ok(s') -> Ok(PrefixRecv(env, expr, v', s', line))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | IntCh(s1, s2, line) ->
        match bind1 var v s1, bind1 var v s2 with
        | Ok(p1), Ok(p2) -> Ok(IntCh(p1, p2, line))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | ExtCh(s1, s2, line) ->
        match bind1 var v s1, bind1 var v s2 with
        | Ok(p1), Ok(p2) -> Ok(ExtCh(p1, p2, line))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | Seq(s1, s2, line) ->
        match bind1 var v s1, bind1 var v s2 with
        | Ok(p1), Ok(p2) -> Ok(Seq(p1, p2, line))
        | Error(err), _ -> Error(err)
        | _, Error(err) -> Error(err)
    | If(env, e, s1, s2, line) ->
        match Env.bind1 var v env, bind1 var v s1, bind1 var v s2 with
        | Ok(env), Ok(s1), Ok(s2) -> Ok(If(env, e, s1, s2, line))
        | Error err, _, _ -> Error(err)
        | _, Error err, _ -> Error(err)
        | _, _, Error err -> Error(err)
    | Match(env, e, sm, line) ->
        let smRes =
            Map.fold
                (fun smAccRes ctor (vars, s) ->
                    Result.bind
                        (fun smAcc -> Result.map (fun s -> Map.add ctor (vars, s) smAcc) (bind1 var v s))
                        smAccRes)
                (Ok Map.empty)
                sm in

        match Env.bind1 var v env, smRes with
        | Ok(env), Ok(sm) -> Ok(Match(env, e, sm, line))
        | Error(err), _ -> Error err
        | _, Error(err) -> Error err
    | InterfaceParallel(env, s1, expr, s2, line) ->
        match Env.bind1 var v env, bind1 var v s1, bind1 var v s2 with
        | Ok(env), Ok(s1), Ok(s2) -> Ok(InterfaceParallel(env, s1, expr, s2, line))
        | Error(err), _, _ -> Error err
        | _, Error(err), _ -> Error err
        | _, _, Error(err) -> Error err
    | Hide(env, s, expr, line) ->
        match Env.bind1 var v env, bind1 var v s with
        | Ok(env), Ok(s) -> Ok(Hide(env, s, expr, line))
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
        | Proc.Unwind(nm, e, line) -> Unwind(genv, nm, e, line)
        | Proc.Stop(line) -> Stop(line)
        | Proc.Skip(line) -> Skip(line)
        | Proc.Prefix(e, p', line) -> Prefix(genv, e, ofProc p', line)
        | Proc.PrefixRecv(e, var, p', line) -> PrefixRecv(genv, e, var, ofProc p', line)
        | Proc.IntCh(p1, p2, line) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            IntCh(s1, s2, line)
        | Proc.ExtCh(p1, p2, line) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            ExtCh(s1, s2, line)
        | Proc.Seq(p1, p2, line) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            Seq(s1, s2, line)
        | Proc.If(e, p1, p2, line) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            If(genv, e, s1, s2, line)
        | Proc.Match(e, mp, line) -> Match(genv, e, Map.map (fun _ (varOpts, p) -> (varOpts, ofProc p)) mp, line)
        | Proc.InterfaceParallel(p1, expr, p2, line) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            InterfaceParallel(genv, s1, expr, s2, line)
        | Proc.Interleave(p1, p2, line) ->
            let s1 = ofProc p1 in
            let s2 = ofProc p2 in
            InterfaceParallel(genv, s1, LitEmpty(TSet(TTuple([], __LINE__), __LINE__), (), line), s2, line)
        | Proc.Hide(p, expr, line) -> let s = ofProc p in Hide(genv, s, expr, line)
        | Proc.Guard(e, p, line) -> let s = ofProc p in If(genv, e, s, Stop(line), line)

    ofProc p

type UnwindConfig = { EvalConfig: EvalConfig }
let unwindConfig cfg = { EvalConfig = cfg }

let unwind (cfg: UnwindConfig) (pm: ProcMap<unit>) (cm: CtorMap) (genv: Env) (s0: State) : Result<State, UnwindError> =
    let rec unwind s visited =
        match s with
        | Unwind(env, pn, exprs, line) ->
            if Set.contains pn visited then
                Error(UnwindError.atLine line (Recursion(pn)))
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
                            | Error err -> Error(UnwindError.atLine line (EnvError err))
                        else
                            Error(ArgumentsLengthMismatch(List.length exprs, varOpts))
                    | Error err -> Error(UnwindError.atLine line (EvalError err))
                | None -> Error(UnwindError.atLine line (NoSuchProcess(pn, procIds pm)))
        | _ -> Ok(s)

    unwind s0 Set.empty

let format (genv: Env) (s0: State) : string =
    let formatExpr = Expr.format noAnnotation

    let rec f s =
        match s with
        | Unwind(env, n, exprs, _) ->
            if List.isEmpty exprs then
                $"%s{n}"
            else
                let s = String.concat "" (List.map (fun expr -> $" %s{formatExpr expr}") exprs) in
                $"%s{n}%s{s} env=%s{Env.format genv env}"
        | Stop _ -> "STOP"
        | Skip _ -> "SKIP"
        | Prefix(env, expr, s', _) -> $"(%s{formatExpr expr} -> %s{f s'} env=%s{Env.format genv env})"
        | PrefixRecv(env, expr, var, s', _) -> $"({formatExpr expr}?{var} -> {f s'} env={Env.format genv env})"
        | IntCh(s1, s2, _) -> $"(%s{f s1} ⨅ %s{f s2})"
        | ExtCh(s1, s2, _) -> $"(%s{f s1} □ %s{f s2})"
        | Seq(s1, s2, _) -> $"(%s{f s1} ; %s{f s2})"
        | If(env, expr, s1, s2, _) ->
            $"(if %s{formatExpr expr} then %s{f s1} else %s{f s2}) env=%s{Env.format genv env})"
        | Match(env, expr, sm, _) ->
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
        | InterfaceParallel(env, s1, expr, s2, _) ->
            $"(%s{f s1} ⟦%s{formatExpr expr}⟧ %s{f s2} env=%s{Env.format genv env})"
        | Hide(env, s, expr, _) -> $"(%s{f s} \\\\ %s{formatExpr expr} env=%s{Env.format genv env})"
        | Omega -> "Ω"
        | ErrorState(msg) -> $"(ERROR: %s{msg})"

    f s0
