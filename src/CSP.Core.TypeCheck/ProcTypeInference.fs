module CSP.Core.ProcTypeInference

open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.Type
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.TypeCstrUnification
open CSP.Core.ExprTypeInference
open CSP.Core.Util

let infer (cm: CtorMap) (tcenv: TypeCstrEnv) (p: Proc<unit>) (s: State) : Result<Proc<TypeCstr> * State, TypeError> =
    let exprInfer = infer cm in

    let rec procInfer tcenv p s =

        match p with
        | Unwind(pn, exprs, line) ->
            let exprsRes =
                List.foldBack
                    (fun expr ->
                        Result.bind (fun (exprs, s) ->
                            Result.map (fun (expr, s) -> (expr :: exprs, s)) (exprInfer tcenv expr s)))
                    exprs
                    (Ok([], s))

            match exprsRes with
            | Ok(exprs, s) -> Ok(Unwind(pn, exprs, line), s)
            | Error terr -> Error(atLine line terr)
        | Stop(line) -> Ok(Stop(line), s)
        | Skip(line) -> Ok(Skip(line), s)
        | Prefix(expr, p, line) ->
            match exprInfer tcenv expr s with
            | Ok(expr, s) -> Result.map (fun (p, s) -> (Prefix(expr, p, line), s)) (procInfer tcenv p s)
            | Error(terr) -> Error(atLine line terr)
        | PrefixRecv(expr, var, p, line) ->
            match exprInfer tcenv expr s with
            | Ok(expr, s) ->
                let tc = Expr.get expr in
                let tcenvRes = Result.mapError TypeEnvError (bind1 var tc tcenv) in

                Result.bind
                    (fun tcenv -> Result.map (fun (p, s) -> (PrefixRecv(expr, var, p, line), s)) (procInfer tcenv p s))
                    tcenvRes
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

                let ctorOpt =
                    Map.fold
                        (fun acc ctorOpt _ ->
                            match acc with
                            | None -> ctorOpt
                            | Some(ctor) -> Some ctor)
                        None
                        pm

                match ctorOpt with
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
                            let pmRes =
                                (Map.fold
                                    (fun accRes ctorOpt (varOpts, p) ->
                                        Result.bind
                                            (fun (pm, s) ->
                                                match ctorOpt with
                                                | Some ctor ->
                                                    match Map.tryFind ctor ctm with
                                                    | None -> Error(NoSuchCtor ctor)
                                                    | Some(tcs) ->
                                                        if List.length varOpts = List.length tcs then
                                                            let tcenvRes =
                                                                Result.mapError
                                                                    TypeEnvError
                                                                    (bindAll (List.zip varOpts tcs) tcenv) in

                                                            (Result.bind
                                                                (fun tcenv ->
                                                                    (Result.bind
                                                                        (fun (pm, s) ->
                                                                            Result.map
                                                                                (fun (p, s) ->
                                                                                    (Map.add
                                                                                        (Some ctor)
                                                                                        (varOpts, p)
                                                                                        pm,
                                                                                     s))
                                                                                (procInfer tcenv p s))
                                                                        accRes))
                                                                tcenvRes)
                                                        else
                                                            Error(
                                                                AssociatedValuesLenMismatch(
                                                                    ctor,
                                                                    Set [ List.length varOpts; List.length tcs ]
                                                                )
                                                            )
                                                | None ->
                                                    let tcenvRes =
                                                        match varOpts with
                                                        | [ Some var ] ->
                                                            Result.mapError TypeEnvError (bind1 var tcUnion tcenv)
                                                        | [ None ] -> Ok(tcenv)
                                                        | _ ->
                                                            Error(
                                                                atLine
                                                                    line
                                                                    (DefaultClauseArgumentsLenMustBe1(varOpts))
                                                            )

                                                    Result.bind
                                                        (fun tcenv ->
                                                            Result.map
                                                                (fun (p, s) -> (Map.add None (varOpts, p) pm, s))
                                                                (procInfer tcenv p s))
                                                        tcenvRes)
                                            accRes)
                                    (Ok(Map.empty, s))
                                    pm)

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
