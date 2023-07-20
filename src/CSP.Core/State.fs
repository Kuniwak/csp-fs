module CSP.Core.State

open CSP.Core.Env
open FSharpx.Collections
open CSP.Core.CtorMap
open CSP.Core.Type
open CSP.Core.Var
open CSP.Core.Ctor
open CSP.Core.Expr
open CSP.Core.ProcMap
open CSP.Core.Proc
open CSP.Core.Val

type State =
    | Unwind of Map<Var, Val> * ProcId * Expr option
    | Stop
    | Skip
    | Prefix of Map<Var, Val> * Expr * State
    | PrefixRecv of Map<Var, Val> * Expr * Var * State
    | IntCh of State * State
    | ExtCh of State * State
    | Seq of State * State
    | If of Map<Var, Val> * Expr * State * State
    | Match of Map<Var, Val> * Expr * Map<Ctor, Var option * State> * (Var option * State) option
    | InterfaceParallel of Map<Var, Val> * State * Expr * State
    | Hide of Map<Var, Val> * State * Expr
    | Omega
    | Error of string * State

let rec bind (var: Var) (v: Val) (s: State) =
    match s with
    | Unwind(env, n, eOpt) -> Unwind(Map.add var v env, n, eOpt)
    | Stop -> Stop
    | Skip -> Skip
    | Prefix(env, expr, s') -> Prefix(Map.add var v env, expr, bind var v s')
    | PrefixRecv(env, expr, v', s') -> PrefixRecv(Map.add var v env, expr, v', bind var v s')
    | IntCh(s1, s2) -> IntCh(bind var v s1, bind var v s2)
    | ExtCh(s1, s2) -> ExtCh(bind var v s1, bind var v s2)
    | Seq(s1, s2) -> Seq(bind var v s1, bind var v s2)
    | If(env, e, s1, s2) -> If(Map.add var v env, e, bind var v s1, bind var v s2)
    | Match(env, e, sm, ds) -> Match(Map.add var v env, e, Map.map (fun _ (var', s) -> (var', bind var v s)) sm, ds)
    | InterfaceParallel(env, s1, expr, s2) -> InterfaceParallel(Map.add var v env, bind var v s1, expr, bind var v s2)
    | Hide(env, s, expr) -> Hide(Map.add var v env, bind var v s, expr)
    | Omega -> Omega
    | Error _ -> s

let ofProc (genv: Map<Var, Val>) (p: Proc) : State =
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
            InterfaceParallel(genv, s1, LitEmpty(TSet(TUnit), None, line), s2)
        | Proc.Hide(p, expr, _) -> let s = ofProc p in Hide(genv, s, expr)
        | Proc.Guard(e, p, _) -> let s = ofProc p in If(genv, e, s, Stop)

    ofProc p

let unwind (pm: ProcMap) (cm: CtorMap) (s0: State) : State =
    let rec loop s visited =
        match s with
        | Unwind(env, pn, eOpt) ->
            if Set.contains s visited then
                failwith "circular definition"
            else
                match Map.tryFind pn pm with
                | Some t ->
                    match (t, eOpt) with
                    | (Some var, p), Some e ->
                        let env = Map.add var (eval cm env e) env in
                        loop (ofProc env p) (Set.add (ofProc env p) visited)
                    | (None, p), None ->
                        loop (ofProc env p) (Set.add (ofProc env p) visited)
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
                                (Map.toList pm)) in

                    failwith $"no such process: {pn} in [{ms}]"
        | _ -> s

    loop s0 Set.empty

let format (pm: ProcMap) (cm: CtorMap) (genv: Env) (s0: State) : string =
    let rec f s isTop =
        match s with
        | Unwind(env, n, eOpt) ->
            if isTop then
                f (unwind pm cm s) false
            else
                match eOpt with
                | Some e -> $"{n} {Expr.format e} env={Env.format genv env}"
                | None -> $"{n}"
        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(env, expr, s') -> $"({Expr.format expr} -> {f s' false} env={Env.format genv env})"
        | PrefixRecv(env, expr, var, s') -> $"({Expr.format expr}?{var} -> {f s' false} env={Env.format genv env})"
        | IntCh(s1, s2) -> $"({f s1 false} ⨅ {f s2 false})"
        | ExtCh(s1, s2) -> $"({f s1 false} □ {f s2 false})"
        | Seq(s1, s2) -> $"({f s1 false} ; {f s2 false})"
        | If(env, expr, s1, s2) -> $"(if {Expr.format expr} then {f s1 false} else {f s2 false}) env={Env.format genv env})"
        | Match(env, expr, sm, ds) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, p')) -> $"{c} {v} -> {f p' false}") (Map.toList sm) in

            match ds with
            | Some(Some var, p') ->
                $"(match {Expr.format expr} with {String.concat sep cs'} | {var} -> {f p' false} env={Env.format genv env})"
            | Some(None, p') ->
                $"(match {Expr.format expr} with {String.concat sep cs'} | _ -> {f p' false} env={Env.format genv env})"
            | None -> $"(match {Expr.format expr} with {String.concat sep cs'} env={Env.format genv env})"
        | InterfaceParallel(env, s1, expr, s2) ->
            $"({f s1 false} ⟦{Expr.format expr}⟧ {f s2 false} env={Env.format genv env})"
        | Hide(env, s, expr) -> $"({f s false} \\\\ {Expr.format expr} env={Env.format genv env})"
        | Omega -> "Ω"
        | Error(msg, s) -> $"(ERROR: {msg} at {f s false})"

    f s0 true
