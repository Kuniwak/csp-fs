module CSP.Core.ProcTypeInference

open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.Type
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.TypeCstrUnification
open CSP.Core.ExprTypeInference
open CSP.Core.Util

let infer
    (pm: ProcMap<unit>)
    (cm: CtorMap)
    (tcenv: TypeCstrEnv)
    (p: Proc<unit>)
    (s: State)
    : Result<Proc<TypeCstr> * State, TypeError> =
    let exprInfer = infer cm in

    let rec procInfer tcenv p s =

        match p with
        | Unwind(pn, exprs, line) ->
            match ProcMap.tryFind pn pm with
            | None -> Error(atLine line (NoSuchProcess(pn)))
            | Some(vars, _) ->
                let ts = vars |> List.map snd in
                let tcs, s = generalizeList ts s in

                if List.length vars = List.length exprs then
                    let exprsRes =
                        List.foldBack
                            (fun (expr, tc) accRes ->
                                accRes
                                |> Result.bind (fun (exprs, s) ->
                                    exprInfer tcenv expr s
                                    |> Result.bind (fun (expr, s) ->
                                        unify s tc (Expr.get expr)
                                        |> Result.map (fun (_, s) -> (expr :: exprs, s)))))
                            (List.zip exprs tcs)
                            (Ok([], s))

                    match exprsRes with
                    | Ok(exprs, s) -> Ok(Unwind(pn, exprs, line), s)
                    | Error terr -> Error(atLine line terr)
                else
                    Error(atLine line (ArgumentsLengthMismatch(pn, vars, exprs)))
        | Stop(line) -> Ok(Stop(line), s)
        | Skip(line) -> Ok(Skip(line), s)
        | Prefix(expr, p, line) ->
            match exprInfer tcenv expr s with
            | Ok(expr, s) -> procInfer tcenv p s |> Result.map (fun (p, s) -> (Prefix(expr, p, line), s))
            | Error(terr) -> Error(atLine line terr)
        | PrefixRecv(exprSet, var, p, line) ->
            match exprInfer tcenv exprSet s with
            | Ok(exprSet, s) ->
                let tcSet = Expr.get exprSet in
                let u, s = newUncertainVarId s in

                match unify s tcSet (TCSet(TCUncertain u)) with
                | Ok(tcSet, s) ->
                    match tcSet with
                    | TCSet tcElem ->
                        let tcenv = bind1 var tcElem tcenv in

                        procInfer tcenv p s
                        |> Result.map (fun (p, s) -> (PrefixRecv(exprSet, var, p, line), s))
                    | _ -> failwith ""
                | Error(err) -> Error(atLine line err)
            | Error(terr) -> Error(atLine line terr)
        | IntCh(p1, p2, line) ->
            match procInfer tcenv p1 s with
            | Ok(p1, s) ->
                match procInfer tcenv p2 s with
                | Ok(p2, s) -> Ok(IntCh(p1, p2, line), s)
                | Error(terr) -> Error(atLine line terr)
            | Error(terr) -> Error(atLine line terr)
        | ExtCh(p1, p2, line) ->
            match procInfer tcenv p1 s with
            | Ok(p1, s) ->
                match procInfer tcenv p2 s with
                | Ok(p2, s) -> Ok(ExtCh(p1, p2, line), s)
                | Error(terr) -> Error(atLine line terr)
            | Error(terr) -> Error(atLine line terr)
        | Seq(p1, p2, line) ->
            match procInfer tcenv p1 s with
            | Ok(p1, s) ->
                match procInfer tcenv p2 s with
                | Ok(p2, s) -> Ok(Seq(p1, p2, line), s)
                | Error(terr) -> Error(atLine line terr)
            | Error(terr) -> Error(atLine line terr)
        | If(expr, p1, p2, line) ->
            match exprInfer tcenv expr s with
            | Ok(expr, s) ->
                match procInfer tcenv p1 s with
                | Ok(p1, s) ->
                    match procInfer tcenv p2 s with
                    | Ok(p2, s) -> Ok(If(expr, p1, p2, line), s)
                    | Error(terr) -> Error(atLine line terr)
                | Error(terr) -> Error(atLine line terr)
            | Error terr -> Error(atLine line terr)
        | Match(exprUnion, pm, line) ->
            match exprInfer tcenv exprUnion s with
            | Ok(exprUnion, s) ->
                let tcUnion = Expr.get exprUnion in

                let tryFindAnyCtor pm =
                    pm
                    |> Map.fold
                        (fun acc ctorOpt _ ->
                            match acc with
                            | None -> ctorOpt
                            | Some(ctor) -> Some ctor)
                        None

                match tryFindAnyCtor pm with
                | None -> Error(atLine line NoCtors)
                | Some(ctor) ->
                    match Map.tryFind ctor cm with
                    | None -> Error(atLine line (NoSuchCtor(ctor)))
                    | Some(un, ctm) ->
                        let ctm, s = generalizeMap ctm s in
                        let tcUnion' = TCUnion(un, ctm) in

                        match unify s tcUnion tcUnion' with
                        | Error(err) -> Error(atLine line err)
                        | Ok(_, s) ->
                            Map.fold
                                (fun accRes ctorOpt (varOpts, p) ->
                                    accRes
                                    |> Result.bind (fun (pm, s) ->
                                        match ctorOpt with
                                        | Some ctor ->
                                            match Map.tryFind ctor ctm with
                                            | None -> Error(NoSuchCtor ctor)
                                            | Some(tcs) ->
                                                if List.length varOpts = List.length tcs then
                                                    let tcenv = bindAllOpts (List.zip varOpts tcs) tcenv in

                                                    accRes
                                                    |> Result.bind (fun (pm, s) ->
                                                        procInfer tcenv p s
                                                        |> Result.map (fun (p, s) ->
                                                            (Map.add (Some ctor) (varOpts, p) pm, s)))
                                                else
                                                    Error(
                                                        AssociatedValuesLenMismatch(
                                                            ctor,
                                                            Set [ List.length tcs; List.length varOpts ]
                                                        )
                                                    )
                                        | None ->
                                            match varOpts with
                                            | [ varOpt ] -> Ok(bind1Opt varOpt tcUnion tcenv)
                                            | _ -> Error(atLine line (DefaultClauseArgumentsLenMustBe1(varOpts)))
                                            |> Result.bind (fun tcenv ->
                                                procInfer tcenv p s
                                                |> Result.map (fun (p, s) -> (Map.add None (varOpts, p) pm, s)))))
                                (Ok(Map.empty, s))
                                pm
                            |> fun pmRes ->
                                match pmRes with
                                | Ok(pm, s) -> Ok(Match(exprUnion, pm, line), s)
                                | Error(terr) -> Error(atLine line terr)
            | Error terr -> Error(atLine line terr)
        | InterfaceParallel(p1, expr, p2, line) ->
            match exprInfer tcenv expr s with
            | Ok(expr, s) ->
                match procInfer tcenv p1 s with
                | Ok(p1, s) ->
                    match procInfer tcenv p2 s with
                    | Ok(p2, s) -> Ok(InterfaceParallel(p1, expr, p2, line), s)
                    | Error(terr) -> Error(atLine line terr)
                | Error(terr) -> Error(atLine line terr)
            | Error terr -> Error(atLine line terr)
        | Interleave(p1, p2, line) ->
            match procInfer tcenv p1 s with
            | Ok(p1, s) ->
                match procInfer tcenv p2 s with
                | Ok(p2, s) -> Ok(Interleave(p1, p2, line), s)
                | Error(terr) -> Error(atLine line terr)
            | Error(terr) -> Error(atLine line terr)
        | Hide(p, expr, line) ->
            match exprInfer tcenv expr s with
            | Ok(expr, s) ->
                match procInfer tcenv p s with
                | Ok(p, s) -> Ok(Hide(p, expr, line), s)
                | Error terr -> Error(atLine line terr)
            | Error(terr) -> Error(atLine line terr)
        | Guard(expr, p, line) ->
            match exprInfer tcenv expr s with
            | Ok(expr, s) ->
                match procInfer tcenv p s with
                | Ok(p, s) -> Ok(Guard(expr, p, line), s)
                | Error terr -> Error(atLine line terr)
            | Error(terr) -> Error(atLine line terr)

    procInfer tcenv p s

let resolve (s: State) (p: Proc<TypeCstr>) : Result<Proc<TypeCstr>, TypeError> =
    let p = map (resolve s >> Result.map Expr.get) p in

    match error p with
    | Some(terr) -> Error(terr)
    | None -> Ok(map (Expr.get >> ResultEx.get format) p)

let instantiate (p: Proc<TypeCstr>) : Proc<Type> =
    map (Expr.get >> TypeCstrInstantiation.instantiate) p

let postProcess (res: Result<Proc<TypeCstr> * State, TypeError>) : Result<Proc<Type> * State, TypeError> =
    Result.map
        (fun (p, s) -> (instantiate p, s))
        (Result.bind (fun (p, s) -> Result.map (fun p -> (p, s)) (resolve s p)) res)
