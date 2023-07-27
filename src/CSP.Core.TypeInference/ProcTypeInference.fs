module CSP.Core.ProcTypeInference

open CSP.Core.CtorMap
open CSP.Core.Proc
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeCstr
open CSP.Core.TypeError

let infer (cm: CtorMap) (tcenv: TypeCstrEnv) (p: Proc) : TypeError option =
    let exprInfer = ExprTypeInference.infer cm in

    let rec procInfer tcenv p s =

        match p with
        | Unwind(_, exprOpt, line) ->
            match exprOpt with
            | Some expr ->
                match exprInfer tcenv expr s with
                | Ok(_, s) -> Ok(s)
                | Error terr -> Error(atLine line terr)
            | None -> Ok(s)
        | Stop _ -> Ok(s)
        | Skip _ -> Ok(s)
        | Prefix(expr, p, line) ->
            match exprInfer tcenv expr s with
            | Ok(_, s) -> procInfer tcenv p s
            | Error(terr) -> Error(atLine line terr)
        | PrefixRecv(expr, var, p, line) ->
            match exprInfer tcenv expr s with
            | Ok(expr, s) ->
                let tc = Expr.get expr in
                let tcenvRes = Result.mapError TypeEnvError (bind1 var tc tcenv) in
                Result.bind (fun tcenv -> procInfer tcenv p s) tcenvRes
            | Error(terr) -> Error(atLine line terr)
        | IntCh(p1, p2, line) -> Result.mapError (atLine line) (Result.bind (procInfer tcenv p2) (procInfer tcenv p1 s))
        | ExtCh(p1, p2, line) -> Result.mapError (atLine line) (Result.bind (procInfer tcenv p2) (procInfer tcenv p1 s))
        | Seq(p1, p2, line) -> Result.mapError (atLine line) (Result.bind (procInfer tcenv p2) (procInfer tcenv p1 s))
        | If(expr, p1, p2, line) ->
            match exprInfer tcenv expr s with
            | Ok(_, s) -> Result.mapError (atLine line) (Result.bind (procInfer tcenv p2) (procInfer tcenv p1 s))
            | Error terr -> Error(atLine line terr)
        | Match(exprUnion, pm, line) ->
            match exprInfer tcenv exprUnion s with
            | Ok(exprUnion, s) ->
                let tcUnion = Expr.get exprUnion in

                match tcUnion with
                | TCUnion(_, cm) ->
                    Result.mapError
                        (atLine line)
                        (Map.fold
                            (fun sRes ctorOpt (varOpts, p) ->
                                match ctorOpt with
                                | Some ctor ->
                                    match Map.tryFind ctor cm with
                                    | None -> Error(atLine line (NoSuchCtor ctor))
                                    | Some(tcs) ->
                                        if List.length varOpts = List.length tcs then
                                            let tcenvRes =
                                                Result.mapError TypeEnvError (bindAll (List.zip varOpts tcs) tcenv) in

                                            (Result.bind (fun tcenv -> Result.bind (procInfer tcenv p) sRes) tcenvRes)
                                        else
                                            Error(
                                                atLine
                                                    line
                                                    (UnionValueLenMismatch(ctor, List.length varOpts, List.length tcs))
                                            )
                                | None ->
                                    let tcenvRes =
                                        match varOpts with
                                        | [ Some var ] -> Result.mapError TypeEnvError (bind1 var tcUnion tcenv)
                                        | [ None ] -> Ok(tcenv)
                                        | _ -> Error(atLine line (DefaultClauseArgumentsLenMustBe1(varOpts)))

                                    Result.bind (fun tcenv -> Result.bind (procInfer tcenv p) sRes) tcenvRes)
                            (Ok(s))
                            pm)
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

    match procInfer tcenv p TypeInference.init with
    | Ok _ -> None
    | Error(terr) -> Some terr
