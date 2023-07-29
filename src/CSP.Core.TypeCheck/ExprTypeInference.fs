module CSP.Core.ExprTypeInference

open CSP.Core.Util
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeCstr
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.TypeCstrResolution
open CSP.Core.TypeCstrUnification
open CSP.Core.TypeCstrInstantiation

let infer
    (cm: CtorMap)
    (tcenv: TypeCstrEnv)
    (expr: Expr<unit>)
    (s0: State)
    : Result<Expr<TypeCstr> * State, TypeError> =
    let rec infer s tcenv expr =
        match expr with
        | LitUnit(_, line) -> Ok(LitUnit(TCUnit, line), s)
        | LitTrue(_, line) -> Ok(LitTrue(TCBool, line), s)
        | LitFalse(_, line) -> Ok(LitFalse(TCBool, line), s)
        | LitNat(n, _, line) -> Ok(LitNat(n, TCNat, line), s)
        | LitEmpty(t, _, line) ->
            if ClassEmpty.derivedBy t then
                let tc, s = generalize t s in Ok(LitEmpty(t, tc, line), s)
            else
                let tc, _ = generalize t s in Error(atLine line (TypeNotDerived(tc, ClassEmpty.name)))
        | Union(ctor, exprs, _, line) ->
            match Map.tryFind ctor cm with
            | None -> Error(NoSuchCtor ctor)
            | Some(un, cm) ->
                match Map.tryFind ctor cm with
                | None -> Error(NoSuchCtor ctor)
                | Some ts ->
                    if List.length ts = List.length exprs then
                        let cm, s =
                            Map.fold
                                (fun (cm, s) ctor ts -> let tcs, s = generalizeList ts s in (Map.add ctor tcs cm, s))
                                (Map.empty, s)
                                cm in

                        let tcs = Map.find ctor cm in
                        let t = TCUnion(un, cm) in

                        let exprsRes =
                            List.foldBack
                                (fun (tc, expr) exprsRes ->
                                    match exprsRes with
                                    | Error terr -> Error terr
                                    | Ok(exprs, s) ->
                                        match infer s tcenv expr with
                                        | Error terr -> Error(atLine line terr)
                                        | Ok(expr, s) ->
                                            let tc' = get expr in

                                            match unify s tc tc' with
                                            | Error terr -> Error(atLine line terr)
                                            | Ok(_, s) -> Ok(expr :: exprs, s))
                                (List.zip tcs exprs)
                                (Ok([], s)) in

                        Result.map (fun (exprs, s) -> (Union(ctor, exprs, t, line), s)) exprsRes
                    else
                        Error(AssociatedValuesLenMismatch(ctor, Set [ List.length exprs; List.length ts ]))

        | If(exprCond, exprThen, exprElse, _, line) ->
            match infer s tcenv exprCond with
            | Error terr -> Error(atLine line terr)
            | Ok(exprCond, s) ->
                let tcCond = get exprCond in

                match unify s tcCond TCBool with
                | Error terr -> Error(atLine line terr)
                | Ok(_, s) ->
                    match infer s tcenv exprThen with
                    | Error terr -> Error(atLine line terr)
                    | Ok(exprThen, s) ->
                        let tcThen = get exprThen in

                        match infer s tcenv exprElse with
                        | Error terr -> Error(atLine line terr)
                        | Ok(exprElse, s) ->
                            let tcElse = get exprElse in

                            match unify s tcThen tcElse with
                            | Error terr -> Error(atLine line terr)
                            | Ok(t, s) -> Ok(If(exprCond, exprThen, exprElse, t, line), s)
        | Match(exprUnion, exprMap, _, line) ->
            match infer s tcenv exprUnion with
            | Error terr -> Error(atLine line terr)
            | Ok(exprUnion, s) ->
                let tUnion = get exprUnion in

                let ctorOpt =
                    Map.fold
                        (fun acc ctorOpt _ ->
                            match acc with
                            | None -> ctorOpt
                            | Some ctor -> Some ctor)
                        None
                        exprMap

                match ctorOpt with
                | None -> Error(atLine line NoCtors)
                | Some ctor ->
                    match Map.tryFind ctor cm with
                    | None -> Error(atLine line (NoSuchCtor ctor))
                    | Some(un, ctm) ->
                        let ctm, s = generalizeMap ctm s in
                        let tUnion' = TCUnion(un, ctm) in

                        match unify s tUnion tUnion' with
                        | Error(terr) -> Error(atLine line terr)
                        | Ok(tUnion, s) ->
                            let inferClause s ctorOpt varOpts expr : Result<Expr<TypeCstr> * State, TypeError> =
                                match ctorOpt with
                                | Some ctor ->
                                    match Map.tryFind ctor ctm with
                                    | None -> Error(atLine line (NoSuchCtor ctor))
                                    | Some tcs ->
                                        if List.length tcs = List.length varOpts then
                                            match bindAll (List.zip varOpts tcs) tcenv with
                                            | Error(terr) -> Error(atLine line (TypeEnvError terr))
                                            | Ok(tenv) ->
                                                match infer s tenv expr with
                                                | Error terr -> Error(atLine line terr)
                                                | Ok(expr, s) -> Ok(expr, s)
                                        else
                                            Error(
                                                AssociatedValuesLenMismatch(
                                                    ctor,
                                                    Set [ List.length varOpts; List.length tcs ]
                                                )
                                            )
                                | None ->
                                    Result.bind
                                        (fun tenv ->
                                            match infer s tenv expr with
                                            | Error terr -> Error(atLine line terr)
                                            | Ok(expr, s) -> Ok(expr, s))
                                        (match List.length varOpts with
                                         | 1 ->
                                             match List.head varOpts with
                                             | Some var ->
                                                 match bind1 var tUnion tcenv with
                                                 | Ok(tenv) -> Ok(tenv)
                                                 | Error(terr) -> Error(atLine line (TypeEnvError terr))
                                             | None -> Ok(tcenv)
                                         | _ -> Error(DefaultClauseArgumentsLenMustBe1(varOpts)))


                            let accRes =
                                MapEx.tryFold1
                                    (fun accRes ctorOpt (vars, expr) ->
                                        match accRes with
                                        | Error s -> Error s
                                        | Ok(tc, exprMap, s) ->
                                            Result.bind
                                                (fun (expr, s) ->
                                                    match unify s tc (get expr) with
                                                    | Error terr -> Error(atLine line terr)
                                                    | Ok(tc, s) -> Ok(tc, Map.add ctorOpt (vars, expr) exprMap, s))
                                                (inferClause s ctorOpt vars expr))
                                    (fun ctorOpt (vars, expr) ->
                                        Result.map
                                            (fun (expr, s) -> (get expr, Map [ (ctorOpt, (vars, expr)) ], s))
                                            (inferClause s ctorOpt vars expr))
                                    exprMap in

                            match accRes with
                            | None -> Error(atLine line NoCtors)
                            | Some(Error terr) -> Error(atLine line terr)
                            | Some(Ok(tc, exprMap, s)) ->
                                let hasDefaultClause = Map.containsKey None exprMap in

                                let keysFromExprMap =
                                    Set(
                                        Seq.collect
                                            (fun ctorOpt ->
                                                match ctorOpt with
                                                | Some ctor -> Seq.singleton ctor
                                                | None -> Seq.empty)
                                            (Map.keys exprMap)
                                    ) in

                                let keysFromTypeMap = Set(Map.keys ctm) in

                                if hasDefaultClause || (keysFromExprMap = keysFromTypeMap) then
                                    Ok(Match(exprUnion, exprMap, tc, line), s)
                                else if Set.isSubset keysFromExprMap keysFromTypeMap then
                                    Error(NotExhausted(Set.difference keysFromTypeMap keysFromExprMap))
                                else
                                    Error(CtorsMismatch(keysFromExprMap, keysFromTypeMap))
        | VarRef(var, _, line) ->
            match tryFind var tcenv with
            | Ok(tc) -> Ok(VarRef(var, tc, line), s)
            | Error(terr) -> Error(atLine line (TypeEnvError terr))
        | Eq(t, expr1, expr2, _, line) ->
            if ClassEq.derivedBy t then
                match infer s tcenv expr1 with
                | Error terr -> Error(atLine line terr)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tcenv expr2 with
                    | Error terr -> Error(atLine line terr)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine line terr)
                        | Ok(tcEq, s) ->
                            let tc, s = generalize t s in

                            match unify s tcEq tc with
                            | Error terr -> Error(atLine line terr)
                            | Ok(_, s) -> Ok(Eq(t, expr1, expr2, TCBool, line), s)
            else
                let tc, _ = generalize t s in Error(atLine line (TypeNotDerived(tc, ClassEq.name)))
        | BoolNot(expr, _, line) ->
            match infer s tcenv expr with
            | Error terr -> Error(atLine line terr)
            | Ok(expr, s) ->
                let t = get expr in

                match unify s t TCBool with
                | Error terr -> Error(atLine line terr)
                | Ok(t, s) -> Ok(BoolNot(expr, t, line), s)
        | Less(t, expr1, expr2, _, line) ->
            if ClassOrd.derivedBy t then
                match infer s tcenv expr1 with
                | Error terr -> Error(atLine line terr)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tcenv expr2 with
                    | Error terr -> Error(atLine line terr)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine line terr)
                        | Ok(tcLess, s) ->
                            let tc, s = generalize t s in

                            match unify s tcLess tc with
                            | Error terr -> Error(atLine line terr)
                            | Ok(_, s) -> Ok(Less(t, expr1, expr2, TCBool, line), s)
            else
                let tc, _ = generalize t s in Error(atLine line (TypeNotDerived(tc, ClassOrd.name)))
        | Plus(t, expr1, expr2, _, line) ->
            if ClassPlus.derivedBy t then
                match infer s tcenv expr1 with
                | Error terr -> Error(atLine line terr)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tcenv expr2 with
                    | Error terr -> Error(atLine line terr)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine line terr)
                        | Ok(tcPlus, s) ->
                            let tc, s = generalize t s in

                            match unify s tcPlus tc with
                            | Error terr -> Error(atLine line terr)
                            | Ok(tcPlus, s) -> Ok(Plus(t, expr1, expr2, tcPlus, line), s)
            else
                let t, _ = generalize t s in Error(atLine line (TypeNotDerived(t, ClassPlus.name)))
        | Minus(t, expr1, expr2, _, line) ->
            if ClassMinus.derivedBy t then
                match infer s tcenv expr1 with
                | Error terr -> Error(atLine line terr)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tcenv expr2 with
                    | Error terr -> Error(atLine line terr)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine line terr)
                        | Ok(tcMinus, s) ->
                            let tc, s = generalize t s in

                            match unify s tcMinus tc with
                            | Error terr -> Error(atLine line terr)
                            | Ok(tcMinus, s) -> Ok(Minus(t, expr1, expr2, tcMinus, line), s)
            else
                let t, _ = generalize t s in Error(atLine line (TypeNotDerived(t, ClassMinus.name)))
        | Times(t, expr1, expr2, _, line) ->
            if ClassTimes.derivedBy t then
                match infer s tcenv expr1 with
                | Error terr -> Error(atLine line terr)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tcenv expr2 with
                    | Error terr -> Error(atLine line terr)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine line terr)
                        | Ok(tcTimes, s) ->
                            let tc, s = generalize t s in

                            match unify s tcTimes tc with
                            | Error terr -> Error(atLine line terr)
                            | Ok(tcTimes, s) -> Ok(Times(t, expr1, expr2, tcTimes, line), s)
            else
                let t, _ = generalize t s in Error(atLine line (TypeNotDerived(t, ClassTimes.name)))
        | Size(t, expr, _, line) ->
            if ClassSize.derivedBy t then
                match infer s tcenv expr with
                | Error terr -> Error(atLine line terr)
                | Ok(expr, s) ->
                    let tc, s = generalize t s in
                    let tc' = get expr in

                    match unify s tc tc' with
                    | Error terr -> Error(atLine line terr)
                    | Ok(_, s) -> Ok(Size(t, expr, TCNat, line), s)
            else
                let t, _ = generalize t s in Error(atLine line (TypeNotDerived(t, ClassSize.name)))
        | Filter(t, var, expr1, expr2, _, line) ->
            if ClassEnum.derivedBy t then
                let tcElem, s =
                    match t with
                    | TSet(tElem) -> generalize tElem s
                    | TList(tElem) -> generalize tElem s
                    | TMap(tK, _) -> generalize tK s
                    | _ -> failwith $"cannot get element type: %s{Type.format true t}" in

                match bind1 var tcElem tcenv with
                | Error(terr) -> Error(atLine line (TypeEnvError terr))
                | Ok(tenv) ->
                    match infer s tenv expr1 with
                    | Error terr -> Error(atLine line terr)
                    | Ok(expr1, s) ->
                        let tc1 = get expr1 in

                        match unify s tc1 TCBool with
                        | Error terr -> Error(atLine line terr)
                        | Ok(_, s) ->
                            match infer s tenv expr2 with
                            | Error terr -> Error(atLine line terr)
                            | Ok(expr2, s) ->
                                let tc, s = generalize t s in
                                let tc2 = get expr2 in

                                match unify s tc2 tc with
                                | Error terr -> Error(atLine line terr)
                                | Ok(tcFilter, s) -> Ok(Filter(t, var, expr1, expr2, tcFilter, line), s)
            else
                let t, _ = generalize t s in Error(atLine line (TypeNotDerived(t, ClassEnum.name)))
        | Exists(t, var, expr1, expr2, _, line) ->
            if ClassEnum.derivedBy t then
                let tcElem, s =
                    match t with
                    | TSet(tElem) -> generalize tElem s
                    | TList(tElem) -> generalize tElem s
                    | TMap(tK, _) -> generalize tK s
                    | _ -> failwith $"cannot get element type: %s{Type.format true t}" in

                match bind1 var tcElem tcenv with
                | Error(terr) -> Error(atLine line (TypeEnvError terr))
                | Ok(tenv) ->
                    match infer s tenv expr1 with
                    | Error terr -> Error(atLine line terr)
                    | Ok(expr1, s) ->
                        let tc1 = get expr1 in

                        match unify s tc1 TCBool with
                        | Error terr -> Error(atLine line terr)
                        | Ok(_, s) ->
                            match infer s tenv expr2 with
                            | Error terr -> Error(atLine line terr)
                            | Ok(expr2, s) ->
                                let tc2 = get expr2 in
                                let tc, s = generalize t s in

                                match unify s tc2 tc with
                                | Error terr -> Error(atLine line terr)
                                | Ok(_, s) -> Ok(Exists(t, var, expr1, expr2, TCBool, line), s)
            else
                let t, _ = generalize t s in Error(atLine line (TypeNotDerived(t, ClassEnum.name)))
        | Tuple(expr1, expr2, _, line) ->
            Result.bind
                (fun (expr1, s) ->
                    Result.map
                        (fun (expr2, s) -> (Tuple(expr1, expr2, TCTuple(get expr1, get expr2), line), s))
                        (infer s tcenv expr2))
                (infer s tcenv expr1)
        | TupleFst(exprTuple, _, line) ->
            match infer s tcenv exprTuple with
            | Error terr -> Error(atLine line terr)
            | Ok(exprTuple, s) ->
                let tcTuple = get exprTuple in
                let uL, s = newUncertainVarId s in
                let uR, s = newUncertainVarId s in

                match unify s tcTuple (TCTuple(TCUncertain uL, TCUncertain uR)) with
                | Ok(TCTuple(tcL, _), s) -> Ok(TupleFst(exprTuple, tcL, line), s)
                | Ok(v) -> failwith $"unification between TTuple and any must return TTuple, but come: %A{v}"
                | Error(err) -> Error(atLine line err)
        | TupleSnd(exprTuple, _, line) ->
            match infer s tcenv exprTuple with
            | Error terr -> Error(atLine line terr)
            | Ok(exprTuple, s) ->
                let tcTuple = get exprTuple in
                let uL, s = newUncertainVarId s in
                let uR, s = newUncertainVarId s in

                match unify s tcTuple (TCTuple(TCUncertain uL, TCUncertain uR)) with
                | Ok(TCTuple(_, tcR), s) -> Ok(TupleSnd(exprTuple, tcR, line), s)
                | Ok(v) -> failwith $"unification between TTuple and any must return TTuple, but come: %A{v}"
                | Error(err) -> Error(atLine line err)
        | ListCons(exprElem, exprList, _, line) ->
            match infer s tcenv exprElem with
            | Error terr -> Error(atLine line terr)
            | Ok(exprElem, s) ->
                let tcElem = get exprElem in

                match infer s tcenv exprList with
                | Error terr -> Error(atLine line terr)
                | Ok(exprList, s) ->
                    let tcList = get exprList in

                    match unify s tcList (TCList tcElem) with
                    | Error terr -> Error(atLine line terr)
                    | Ok(tcList, s) -> Ok(ListCons(exprElem, exprList, tcList, line), s)
        | ListNth(exprList, exprIdx, _, line) ->
            match infer s tcenv exprList with
            | Error terr -> Error(atLine line terr)
            | Ok(exprList, s) ->
                let tcList = get exprList in
                let u, s = newUncertainVarId s in

                match unify s tcList (TCList(TCUncertain u)) with
                | Error terr -> Error(atLine line terr)
                | Ok(TCList(tcElem), s) ->
                    match infer s tcenv exprIdx with
                    | Error terr -> Error(atLine line terr)
                    | Ok(exprIdx, s) ->
                        let tcIdx = get exprIdx in

                        match unify s tcIdx TCNat with
                        | Error terr -> Error(atLine line terr)
                        | Ok(_, s) -> Ok(ListNth(exprList, exprIdx, tcElem, line), s)
                | x -> failwith $"unification between TList and any must return TList, but come: %A{x}"
        | SetRange(exprLower, exprUpper, _, line) ->
            match infer s tcenv exprLower with
            | Error terr -> Error(atLine line terr)
            | Ok(exprLower, s) ->
                let tcLower = get exprLower in

                match unify s tcLower TCNat with
                | Error terr -> Error(atLine line terr)
                | Ok(_, s) ->
                    match infer s tcenv exprUpper with
                    | Error terr -> Error(atLine line terr)
                    | Ok(exprUpper, s) ->
                        let tcUpper = get exprUpper in

                        match unify s tcUpper TCNat with
                        | Error terr -> Error(atLine line terr)
                        | Ok(_, s) -> Ok(ListNth(exprLower, exprUpper, TCSet(TCNat), line), s)

        | SetInsert(exprElem, exprSet, _, line) ->
            match infer s tcenv exprElem with
            | Error terr -> Error(atLine line terr)
            | Ok(exprElem, s) ->
                let tcElem = get exprElem in

                match infer s tcenv exprSet with
                | Error terr -> Error(atLine line terr)
                | Ok(exprSet, s) ->
                    let tcSet = get exprSet in

                    match unify s tcSet (TCSet(tcElem)) with
                    | Error terr -> Error(atLine line terr)
                    | Ok(tcSet, s) -> Ok(SetInsert(exprElem, exprSet, tcSet, line), s)

        | SetRemove(exprElem, exprSet, _, line) ->
            match infer s tcenv exprElem with
            | Error terr -> Error(atLine line terr)
            | Ok(exprElem, s) ->
                let tcElem = get exprElem in

                match infer s tcenv exprSet with
                | Error terr -> Error(atLine line terr)
                | Ok(exprSet, s) ->
                    let tcSet = get exprSet in

                    match unify s tcSet (TCSet(tcElem)) with
                    | Error terr -> Error(atLine line terr)
                    | Ok(tcSet, s) -> Ok(SetInsert(exprElem, exprSet, tcSet, line), s)

        | SetMem(exprElem, exprSet, _, line) ->
            match infer s tcenv exprElem with
            | Error terr -> Error(atLine line terr)
            | Ok(exprElem, s) ->
                let tcElem = get exprElem in

                match infer s tcenv exprSet with
                | Error terr -> Error(atLine line terr)
                | Ok(exprSet, s) ->
                    let tcSet = get exprSet in

                    match unify s tcSet (TCSet(tcElem)) with
                    | Error terr -> Error(atLine line terr)
                    | Ok(_, s) -> Ok(SetMem(exprElem, exprSet, TCBool, line), s)

        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            match infer s tcenv exprKey with
            | Error terr -> Error(atLine line terr)
            | Ok(exprKey, s) ->
                let tcKey = get exprKey in

                match infer s tcenv exprVal with
                | Error terr -> Error(atLine line terr)
                | Ok(exprVal, s) ->
                    let tcVal = get exprVal in

                    match infer s tcenv exprMap with
                    | Error terr -> Error(atLine line terr)
                    | Ok(exprMap, s) ->
                        let tcMap = get exprMap in

                        match unify s tcMap (TCMap(tcKey, tcVal)) with
                        | Error terr -> Error(atLine line terr)
                        | Ok(tcMap, s) -> Ok(MapAdd(exprKey, exprVal, exprMap, tcMap, line), s)

        | MapFindOpt(exprKey, exprMap, _, line) ->
            match infer s tcenv exprKey with
            | Error terr -> Error(atLine line terr)
            | Ok(exprKey, s) ->
                let tcKey = get exprKey in

                match infer s tcenv exprMap with
                | Error terr -> Error(atLine line terr)
                | Ok(exprMap, s) ->
                    let tcMap = get exprMap in
                    let u, s = newUncertainVarId s in

                    match unify s tcMap (TCMap(tcKey, TCUncertain u)) with
                    | Error terr -> Error(atLine line terr)
                    | Ok(TCMap(_, tcVal), s) ->
                        Ok(
                            MapFindOpt(
                                exprKey,
                                exprMap,
                                TCUnion("option", Map [ (Ctor "Some", [ tcVal ]); Ctor "None", [] ]),
                                line
                            ),
                            s
                        )
                    | x -> failwith $"unification between TMap and any must return TMap, but come: %A{x}"
        | Univ(t, _, line) -> let tc, s = generalize t s in Ok(Univ(t, TCSet tc, line), s)

    infer s0 tcenv expr

let resolve (s: State) (expr: Expr<TypeCstr>) : Result<Expr<TypeCstr>, TypeError> =
    let expr = map (get >> resolve s) expr in

    match error expr with
    | Some(terr) -> Error(terr)
    | None -> Ok(map (fun expr -> ResultEx.get format (get expr)) expr)

let instantiate (expr: Expr<TypeCstr>) : Expr<Type> = map (get >> instantiate) expr

let postProcess (res: Result<Expr<TypeCstr> * State, TypeError>) : Result<Expr<Type> * State, TypeError> =
    Result.bind (fun (expr, s) -> Result.map (fun expr -> (instantiate expr, s)) (resolve s expr)) res
