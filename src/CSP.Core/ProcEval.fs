module CSP.Core.ProcEval

open CSP.Core.Eval
open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.Proc
open CSP.Core.ProcEvalError
open CSP.Core.State
open CSP.Core.UnionMap
open CSP.Core.Val
open CSP.Core.Util

type ProcEvalConfig = { EvalConfig: EvalConfig }

let eval (cfg: ProcEvalConfig) (um: UnionMap) (cm: CtorMap) (env: Env) (p: Proc<'a>) : Result<State, ProcEvalError> =
    let exprEval env expr =
        Result.mapError ExprError (eval cfg.EvalConfig um cm env expr)

    let getBool v =
        match v with
        | VBool(b) -> Ok(b)
        | _ -> Error(ValNotBool(v))

    let getSet v =
        match v with
        | VSet(s) -> Ok(s)
        | _ -> Error(ValNotSet(v))

    let getUnion v =
        match v with
        | VUnion(ctor, vs) -> Ok(ctor, vs)
        | _ -> Error(ValNotUnion(v))

    let rec eval env (p: Proc<'a>) =
        match p with
        | Proc.Unwind(pn, exprs, _) ->
            exprs
            |> ResultEx.bindAll (exprEval env)
            // NOTE: Do not eval recursively Unwind. It make infinite recursion on recursive processes such as `P = a -> P`.
            //       `P = a -> P` is equivalent to infinite process such as `a -> a -> a -> ...`, so we cannot expand it.
            |> Result.map (fun vs -> Unwind(pn, vs))
        | Proc.Skip _ -> Ok(Skip)
        | Proc.Stop _ -> Ok(Stop)
        | Proc.Prefix(expr, p, _) ->
            expr
            |> exprEval env
            |> Result.bind (fun v -> p |> eval env |> Result.map (fun p -> Prefix(v, p)))
        | Proc.PrefixRecv(expr, var, p, _) ->
            expr
            |> exprEval env
            |> Result.bind getSet
            |> Result.map (fun vs -> PrefixRecv(vs, env, var, p))
        | Proc.IntCh(p1, p2, _) -> (p1, p2) |> ResultEx.bind2 (eval env) (eval env) |> Result.map IntCh
        | Proc.ExtCh(p1, p2, _) -> (p1, p2) |> ResultEx.bind2 (eval env) (eval env) |> Result.map ExtCh
        | Proc.Seq(p1, p2, _) -> (p1, p2) |> ResultEx.bind2 (eval env) (eval env) |> Result.map Seq
        | Proc.If(expr, p1, p2, _) ->
            expr
            |> exprEval env
            |> Result.bind getBool
            |> Result.bind (fun b -> if b then eval env p1 else eval env p2)
        | Proc.Match(expr, pm, _) ->
            expr
            |> exprEval env
            |> Result.bind getUnion
            |> Result.bind (fun (ctor, vs) ->
                match Map.tryFind (Some ctor) pm with
                | Some(varOpts, p) ->
                    if List.length varOpts = List.length vs then
                        let env = bindAllOpts (List.zip varOpts vs) env in eval env p
                    else
                        Error(AssociatedValuesLengthMismatch(ctor, varOpts, vs))
                | None ->
                    match Map.tryFind None pm with
                    | None -> Error(NoClauseMatched(ctor, vs))
                    | Some(varOpts, p) ->
                        if List.length varOpts = 1 then
                            let env = bind1Opt (List.head varOpts) (VUnion(ctor, vs)) env in eval env p
                        else
                            Error(DefaultClauseArgumentLenMustBe1(varOpts)))
        | Proc.InterfaceParallel(p1, expr, p2, _) ->
            expr
            |> exprEval env
            |> Result.bind getSet
            |> Result.bind (fun vs ->
                (p1, p2)
                |> ResultEx.bind2 (eval env) (eval env)
                |> Result.map (fun (s1, s2) -> InterfaceParallel(s1, vs, s2)))
        | Interleave(p1, p2, _) ->
            (p1, p2)
            |> ResultEx.bind2 (eval env) (eval env)
            |> Result.map (fun (s1, s2) -> InterfaceParallel(s1, Set.empty, s2))
        | Proc.Hide(p, expr, _) ->
            expr
            |> exprEval env
            |> Result.bind getSet
            |> Result.bind (fun vs -> p |> eval env |> Result.map (fun s -> Hide(s, vs)))
        | Proc.Guard(expr, p, _) ->
            expr
            |> exprEval env
            |> Result.bind getBool
            |> Result.bind (fun b -> if b then eval env p else Ok(Stop))

    eval env p
