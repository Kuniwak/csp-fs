module CSP.Core.ProcTypeInference

open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.TypeInference
open CSP.Core.Var

let infer (cm: CtorMap) (tcenv: TypeCstrEnv) (p: Proc<unit>) (s: State) : Result<Proc<TypeCstr> * State, TypeError> =
    let exprInfer = ExprTypeInference.infer cm in

    let rec procInfer tcenv p s =

        match p with
        | Unwind(pn, exprOpt, line) ->
            match exprOpt with
            | Some expr ->
                match exprInfer tcenv expr s with
                | Ok(expr, s) -> Ok(Unwind(pn, Some expr, line), s)
                | Error terr -> Error(atLine line terr)
            | None -> Ok(Unwind(pn, None, line), s)
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

                match tcUnion with
                | TCUnion(_, cm) ->
                    let pmRes =
                        (Map.fold
                            (fun
                                (accRes: Result<Map<Ctor option, Var option list * Proc<TypeCstr>>, TypeError>)
                                ctorOpt
                                (varOpts, p) ->
                                Result.bind
                                    (fun (pm, s) ->
                                        match ctorOpt with
                                        | Some ctor ->
                                            match Map.tryFind ctor cm with
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
                                                                            (Map.add (Some ctor) (varOpts, p) pm, s))
                                                                        (procInfer tcenv p s))
                                                                accRes))
                                                        tcenvRes)
                                                else
                                                    Error(
                                                        UnionValueLenMismatch(
                                                            ctor,
                                                            List.length varOpts,
                                                            List.length tcs
                                                        )
                                                    )
                                        | None ->
                                            let tcenvRes =
                                                match varOpts with
                                                | [ Some var ] ->
                                                    Result.mapError TypeEnvError (bind1 var tcUnion tcenv)
                                                | [ None ] -> Ok(tcenv)
                                                | _ -> Error(atLine line (DefaultClauseArgumentsLenMustBe1(varOpts)))

                                            Result.bind
                                                (fun tcenv ->
                                                    (Result.bind
                                                        (fun (pm, s) ->
                                                            Result.map
                                                                (fun (p, s) -> (Map.add None (varOpts, p) pm, s))
                                                                (procInfer tcenv p s))
                                                        accRes))
                                                tcenvRes)
                                    accRes)
                            (Ok(Map.empty, s))
                            pm)

                    match pmRes with
                    | Ok(pm, s) -> Ok(Match(exprUnion, pm, line), s)
                    | Error(terr) -> Error(atLine line terr)
                | _ -> Error(atLine line (NotUnion(tcUnion)))
            | Error terr -> Error(atLine line terr)
        | InterfaceParallel(p1, expr, p2, line) ->
            match exprInfer tcenv expr s with
            | Ok(_, s) -> Result.mapError (atLine line) (Result.bind (procInfer tcenv p2) (procInfer tcenv p1 s))
            | Error terr -> Error(atLine line terr)
        | Interleave(p1, p2, line) ->
            Result.mapError (atLine line) (Result.bind (procInfer tcenv p2) (procInfer tcenv p1 s))
        | Hide(p, expr, line) ->
            match exprInfer tcenv expr s with
            | Ok(_, s) -> Result.mapError (atLine line) (procInfer tcenv p s)
            | Error terr -> Error(atLine line terr)
        | Guard(expr, p, line) ->
            match exprInfer tcenv expr s with
            | Ok(_, s) -> Result.mapError (atLine line) (procInfer tcenv p s)
            | Error terr -> Error(atLine line terr)

    procInfer tcenv p s
