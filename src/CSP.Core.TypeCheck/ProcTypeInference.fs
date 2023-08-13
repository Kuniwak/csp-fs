module CSP.Core.ProcTypeInference

open CSP.Core.UnionMap
open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.Type
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.ExhaustivenessChk
open CSP.Core.TypeGeneralization
open CSP.Core.TypeCstrUnification
open CSP.Core.ExprTypeInference
open CSP.Core.Util

let infer
    (pm: ProcMap<unit>)
    (um: UnionMap)
    (cm: CtorMap)
    (tcenv: TypeCstrEnv)
    (p: Proc<unit>)
    (s: State)
    : Result<Proc<TypeCstr> * State, TypeError> =
    let exprInfer = infer um cm in
    let atArgPos i err = At(err, $"argument at %d{i}")

    let rec procInfer tcenv p s =
        match p with
        | Unwind(pn, exprs, line) ->
            ProcMap.tryFind pn pm
            |> Option.map (fun (vars, _) ->
                let ts = vars |> List.map snd in
                let tcs, s, _ = generalizeList ts s Map.empty in

                if List.length vars = List.length exprs then
                    List.foldBack
                        (fun (i, expr, tc) accRes ->
                            accRes
                            |> Result.bind (fun (exprs, s) ->
                                exprInfer tcenv expr s
                                |> Result.bind (fun (expr, s) ->
                                    unify s tc (Expr.get expr) |> Result.map (fun (_, s) -> (expr :: exprs, s)))
                                |> Result.mapError (atArgPos i)))
                        (List.mapi (fun i (expr, tc) -> (i, expr, tc)) (List.zip exprs tcs))
                        (Ok([], s))
                    |> Result.map (fun (exprs, s) -> (Unwind(pn, exprs, line), s))
                else
                    Error(ArgumentsLengthMismatch(pn, vars, exprs)))
            |> Option.defaultValue (Error(NoSuchProcess(pn)))
            |> Result.mapError (atLine line)
        | Stop(line) -> Ok(Stop(line), s)
        | Skip(line) -> Ok(Skip(line), s)
        | Prefix(expr, p, line) ->
            exprInfer tcenv expr s
            |> Result.bind (fun (expr, s) ->
                procInfer tcenv p s |> Result.map (fun (p, s) -> (Prefix(expr, p, line), s)))
            |> Result.mapError (atLine line)
        | PrefixRecv(exprSet, var, p, line) ->
            exprInfer tcenv exprSet s
            |> Result.bind (fun (exprSet, s) ->
                let u, s = newUncertainVarId s in

                unify s (Expr.get exprSet) (TCSet(TCUncertain u))
                |> Result.bind (fun (tcSet, s) ->
                    match tcSet with
                    | TCSet tcElem ->
                        let tcenv = bind1 var tcElem tcenv in

                        procInfer tcenv p s
                        |> Result.map (fun (p, s) -> (PrefixRecv(exprSet, var, p, line), s))
                    | _ -> failwith $"unification between TSet and any must return TSet, but come: %A{tcSet}"))
            |> Result.mapError (atLine line)
        | IntCh(p1, p2, line) ->
            procInfer tcenv p1 s
            |> Result.bind (fun (p1, s) -> procInfer tcenv p2 s |> Result.map (fun (p2, s) -> (IntCh(p1, p2, line), s)))
            |> Result.mapError (atLine line)
        | ExtCh(p1, p2, line) ->
            procInfer tcenv p1 s
            |> Result.bind (fun (p1, s) -> procInfer tcenv p2 s |> Result.map (fun (p2, s) -> (ExtCh(p1, p2, line), s)))
            |> Result.mapError (atLine line)
        | Seq(p1, p2, line) ->
            procInfer tcenv p1 s
            |> Result.bind (fun (p1, s) -> procInfer tcenv p2 s |> Result.map (fun (p2, s) -> (Seq(p1, p2, line), s)))
            |> Result.mapError (atLine line)
        | If(expr, p1, p2, line) ->
            exprInfer tcenv expr s
            |> Result.bind (fun (expr, s) ->
                procInfer tcenv p1 s
                |> Result.bind (fun (p1, s) ->
                    procInfer tcenv p2 s |> Result.map (fun (p2, s) -> (If(expr, p1, p2, line), s))))
            |> Result.mapError (atLine line)
        | Match(exprUnion, procMap, line) ->
            exhaustivenessCheck um cm procMap
            |> Result.bind (fun (un, tVars, tsm) ->
                let tcs, tcsm, s, _ = generalizeMap tsm tVars s Map.empty in

                exprInfer tcenv exprUnion s
                |> Result.bind (fun (exprUnion, s) ->
                    unify s (Expr.get exprUnion) (TCUnion(un, tcs))
                    |> Result.bind (fun (_, s) ->
                        procMap
                        |> Map.fold
                            (fun mRes ctorOpt (varOpts, _) ->
                                mRes
                                |> Result.bind (fun tcenvMap ->
                                    ctorOpt
                                    |> Option.map (fun ctor ->
                                        Map.tryFind ctor tcsm
                                        |> Option.map (fun tcs ->
                                            if List.length tcs = List.length varOpts then
                                                Ok(bindAllOpts (List.zip varOpts tcs) tcenv)
                                            else
                                                Error(
                                                    AssociatedValuesLenMismatch(
                                                        ctor,
                                                        Set [ List.length tcs; List.length varOpts ]
                                                    )
                                                ))
                                        |> Option.defaultValue (
                                            Error(UnionMapError(UnionMapError.NoSuchCtor(un, ctor)))
                                        ))
                                    |> Option.defaultValue (
                                        if List.length varOpts = 1 then
                                            Ok(bindAllOpts (List.zip varOpts [ Expr.get exprUnion ]) tcenv)
                                        else
                                            Error(DefaultClauseArgumentsLenMustBe1(varOpts))
                                    )
                                    |> Result.map (fun tcenv -> Map.add ctorOpt tcenv tcenvMap)))
                            (Ok(Map.empty))
                        |> Result.bind (fun tcenvMap ->
                            procMap
                            |> Map.fold
                                (fun accRes ctorOpt (varOpts, p) ->
                                    accRes
                                    |> Result.bind (fun (procMap, s) ->
                                        let tcenv = Map.find ctorOpt tcenvMap in

                                        procInfer tcenv p s
                                        |> Result.map (fun (p, s) -> (Map.add ctorOpt (varOpts, p) procMap, s))))
                                (Ok(Map.empty, s)))
                        |> Result.map (fun (procMap, s) -> (Match(exprUnion, procMap, line), s)))))
            |> Result.mapError (atLine line)
        | InterfaceParallel(p1, expr, p2, line) ->
            exprInfer tcenv expr s
            |> Result.bind (fun (expr, s) ->
                procInfer tcenv p1 s
                |> Result.bind (fun (p1, s) ->
                    procInfer tcenv p2 s
                    |> Result.map (fun (p2, s) -> (InterfaceParallel(p1, expr, p2, line), s))))
            |> Result.mapError (atLine line)
        | Interleave(p1, p2, line) ->
            procInfer tcenv p1 s
            |> Result.bind (fun (p1, s) ->
                procInfer tcenv p2 s
                |> Result.map (fun (p2, s) -> (Interleave(p1, p2, line), s)))
            |> Result.mapError (atLine line)
        | Hide(p, expr, line) ->
            exprInfer tcenv expr s
            |> Result.bind (fun (expr, s) -> procInfer tcenv p s |> Result.map (fun (p, s) -> Hide(p, expr, line), s))
            |> Result.mapError (atLine line)
        | Guard(expr, p, line) ->
            exprInfer tcenv expr s
            |> Result.bind (fun (expr, s) ->
                procInfer tcenv p s |> Result.map (fun (p, s) -> (Guard(expr, p, line), s)))
            |> Result.mapError (atLine line)

    procInfer tcenv p s

let resolve (s: State) (p: Proc<TypeCstr>) : Result<Proc<TypeCstr>, TypeError> =
    let p = Proc.map (resolve s >> Result.map Expr.get) p in

    match Proc.error p with
    | Some(terr) -> Error(terr)
    | None -> Ok(Proc.map (Expr.get >> ResultEx.get format) p)

let instantiate (p: Proc<TypeCstr>) : Proc<Type> =
    Proc.map (Expr.get >> TypeCstrInstantiation.instantiate) p

let postProcess (res: Result<Proc<TypeCstr> * State, TypeError>) : Result<Proc<Type> * State, TypeError> =
    res
    |> Result.bind (fun (p, s) -> resolve s p |> Result.map (fun p -> (p, s)))
    |> Result.map (fun (p, s) -> (instantiate p, s))
