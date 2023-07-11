module CSP.Core.State

open FSharpx.Collections
open CSP.Core.Type
open CSP.Core.Expr
open CSP.Core.ProcMap
open CSP.Core.Proc
open CSP.Core.Val

type State<'P, 'Var, 'Ctor when 'P: comparison and 'Var: comparison and 'Ctor: comparison> =
    | Unwind of Map<'Var, Val<'Ctor>> * 'P * Expr<'Var, 'Ctor> option
    | Stop
    | Skip
    | Prefix of Map<'Var, Val<'Ctor>> * Expr<'Var, 'Ctor> * State<'P, 'Var, 'Ctor>
    | PrefixRecv of Map<'Var, Val<'Ctor>> * Expr<'Var, 'Ctor> * 'Var * State<'P, 'Var, 'Ctor>
    | IntCh of State<'P, 'Var, 'Ctor> * State<'P, 'Var, 'Ctor>
    | ExtCh of State<'P, 'Var, 'Ctor> * State<'P, 'Var, 'Ctor>
    | Seq of State<'P, 'Var, 'Ctor> * State<'P, 'Var, 'Ctor>
    | If of Map<'Var, Val<'Ctor>> * Expr<'Var, 'Ctor> * State<'P, 'Var, 'Ctor> * State<'P, 'Var, 'Ctor>
    | Match of
        Map<'Var, Val<'Ctor>> *
        Expr<'Var, 'Ctor> *
        Map<Ctor<'Ctor>, 'Var * State<'P, 'Var, 'Ctor>> *
        ('Var option * State<'P, 'Var, 'Ctor>) option
    | InterfaceParallel of Map<'Var, Val<'Ctor>> * State<'P, 'Var, 'Ctor> * Expr<'Var, 'Ctor> * State<'P, 'Var, 'Ctor>
    | Hide of Map<'Var, Val<'Ctor>> * State<'P, 'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Omega
    | Error of string * State<'P, 'Var, 'Ctor>

let rec bind (var: 'Var) (v: Val<'Ctor>) (s: State<'P, 'Var, 'Ctor>) =
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

let rec ofProc
    (m: ProcMap<'P, 'Var, 'Ctor>)
    (genv: Map<'Var, Val<'Ctor>>)
    (p: Proc<'P, 'Var, 'Ctor>)
    : State<'P, 'Var, 'Ctor> =
    match p with
    | Proc.Unwind(n, e) -> Unwind(genv, n, e)
    | Proc.Stop -> Stop
    | Proc.Skip -> Skip
    | Proc.Prefix(e, p') -> Prefix(genv, e, ofProc m genv p')
    | Proc.PrefixRecv(e, var, p') -> PrefixRecv(genv, e, var, ofProc m genv p')
    | Proc.IntCh(p1, p2) -> IntCh(ofProc m genv p1, ofProc m genv p2)
    | Proc.ExtCh(p1, p2) -> ExtCh(ofProc m genv p1, ofProc m genv p2)
    | Proc.Seq(p1, p2) -> Seq(ofProc m genv p1, ofProc m genv p2)
    | Proc.If(e, p1, p2) -> If(genv, e, ofProc m genv p1, ofProc m genv p2)
    | Proc.Match(e, pm, dp) ->
        Match(
            genv,
            e,
            Map.map (fun _ (v, p) -> (v, ofProc m genv p)) pm,
            Option.map (fun (varOpt, s') -> (varOpt, ofProc m genv s')) dp
        )
    | Proc.InterfaceParallel(p1, expr, p2) -> InterfaceParallel(genv, ofProc m genv p1, expr, ofProc m genv p2)
    | Proc.Interleave(p1, p2) -> InterfaceParallel(genv, ofProc m genv p1, SetEmpty, ofProc m genv p2)
    | Proc.Hide(p, expr) -> Hide(genv, ofProc m genv p, expr)
    | Proc.Guard(e, p) -> If(genv, e, ofProc m genv p, Stop)

let unwind (m: ProcMap<'P, 'Var, 'Ctor>) (s0: State<'P, 'Var, 'Ctor>) : State<'P, 'Var, 'Ctor> =
    let rec loop (s: State<'P, 'Var, 'Ctor>) visited =
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

let format (m: ProcMap<'P, 'Var, 'Ctor>) (s0: State<'P, 'Var, 'Ctor>) : string =
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
        | Prefix(env, expr, s') -> $"({Expr.format expr} -> {f s' false} env={Env.format env})"
        | PrefixRecv(env, expr, var, s') -> $"({Expr.format expr}?{var} -> {f s' false} env={Env.format env})"
        | IntCh(s1, s2) -> $"({f s1 false} ⨅ {f s2 false})"
        | ExtCh(s1, s2) -> $"({f s1 false} □ {f s2 false})"
        | Seq(s1, s2) -> $"({f s1 false} ; {f s2 false})"
        | If(env, expr, s1, s2) -> $"(if {Expr.format expr} then {f s1 false} else {f s2 false}) env={Env.format env})"
        | Match(env, expr, sm, ds) ->
            let sep = " | " in
            let cs' = List.map (fun (c, (v, p')) -> $"{c} {v} -> {f p' false}") (Map.toList sm) in

            match ds with
            | Some(Some var, p') ->
                $"(match {Expr.format expr} with {String.concat sep cs'} | {var} -> {f p' false} env={Env.format env})"
            | Some(None, p') ->
                $"(match {Expr.format expr} with {String.concat sep cs'} | _ -> {f p' false} env={Env.format env})"
            | None -> $"(match {Expr.format expr} with {String.concat sep cs'} env={Env.format env})"
        | InterfaceParallel(env, s1, expr, s2) ->
            $"({f s1 false} ⟦{Expr.format expr}⟧ {f s2 false} env={Env.format env})"
        | Hide(env, s, expr) -> $"({f s false} \\\\ {Expr.format expr} env={Env.format env})"
        | Omega -> "Ω"
        | Error(msg, s) -> $"(ERROR: {msg} at {f s false})"

    f s0 true
