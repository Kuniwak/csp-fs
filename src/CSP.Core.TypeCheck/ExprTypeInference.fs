module CSP.Core.ExprTypeInference

open CSP.Core.Util
open CSP.Core.UnionMap
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeCstr
open CSP.Core.CtorMap
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.TypeError
open CSP.Core.ExhaustivenessChk
open CSP.Core.TypeInferenceState
open CSP.Core.TypeGeneralization
open CSP.Core.TypeCstrResolution
open CSP.Core.TypeCstrUnification
open CSP.Core.TypeCstrInstantiation


let infer
    (um: UnionMap)
    (cm: CtorMap)
    (tcenv: TypeCstrEnv)
    (expr: Expr<unit>)
    (s0: State)
    : Result<Expr<TypeCstr> * State, TypeError> =
    let rec infer s tcenv (expr: Expr<unit>) =
        match expr with
        | LitUnit(_, line) -> Ok(LitUnit(TCUnit, line), s)
        | LitTrue(_, line) -> Ok(LitTrue(TCBool, line), s)
        | LitFalse(_, line) -> Ok(LitFalse(TCBool, line), s)
        | LitNat(n, _, line) -> Ok(LitNat(n, TCNat, line), s)
        | LitEmpty(t, _, line) ->
            if ClassEmpty.derivedBy t then
                let tc, s, _ = generalize t s Map.empty in Ok(LitEmpty(t, tc, line), s)
            else
                let tc, _, _ = generalize t s Map.empty in Error(TypeNotDerived(tc, ClassEmpty.name))
            |> Result.mapError (atLine line)
        | Union(ctor, exprs, _, line) ->
            List.foldBack
                (fun expr accRes ->
                    accRes
                    |> Result.bind (fun (tcs, s) -> infer s tcenv expr |> Result.map (fun (tc, s) -> (tc :: tcs, s))))
                exprs
                (Ok([], s))
            |> Result.bind (fun (exprs, s) ->
                generalizeUnion um cm ctor (List.map get exprs) s
                |> Result.map (fun (tc, s) -> (Union(ctor, exprs, tc, line), s)))
            |> Result.mapError (atLine line)
        | If(exprCond, exprThen, exprElse, _, line) ->
            infer s tcenv exprCond
            |> Result.bind (fun (exprCond, s) ->
                unify s (get exprCond) TCBool
                |> Result.bind (fun (_, s) ->
                    infer s tcenv exprThen
                    |> Result.bind (fun (exprThen, s) ->
                        infer s tcenv exprElse
                        |> Result.bind (fun (exprElse, s) ->
                            unify s (get exprThen) (get exprElse)
                            |> Result.map (fun (t, s) -> (If(exprCond, exprThen, exprElse, t, line), s))))))
            |> Result.mapError (atLine line)
        | Match(exprUnion, exprMap, _, line) ->
            exhaustivenessCheck um cm exprMap
            |> Result.bind (fun (un, tVars, tsm) ->
                let tcs, tcm, s, _ = generalizeMap tsm tVars s Map.empty in

                infer s tcenv exprUnion
                |> Result.bind (fun (exprUnion, s) ->
                    unify s (get exprUnion) (TCUnion(un, tcs))
                    |> Result.bind (fun (_, s) ->
                        exprMap
                        |> Map.fold
                            (fun mRes ctorOpt (varOpts, _) ->
                                mRes
                                |> Result.bind (fun tcenvMap ->
                                    ctorOpt
                                    |> Option.map (fun ctor ->
                                        Map.tryFind ctor tcm
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
                                            Ok(bindAllOpts (List.zip varOpts [ get exprUnion ]) tcenv)
                                        else
                                            Error(DefaultClauseArgumentsLenMustBe1(varOpts))
                                    )
                                    |> Result.map (fun tcenv -> Map.add ctorOpt tcenv tcenvMap)))
                            (Ok(Map.empty))
                        |> Result.bind (fun tcenvMap ->
                            let u, s = newUncertainVarId s in

                            exprMap
                            |> Map.fold
                                (fun accRes ctorOpt (varOpts, expr) ->
                                    accRes
                                    |> Result.bind (fun (tc, exprMap, s) ->
                                        let tcenv = Map.find ctorOpt tcenvMap in

                                        infer s tcenv expr
                                        |> Result.bind (fun (expr, s) ->
                                            unify s tc (get expr)
                                            |> Result.map (fun (tc, s) ->
                                                (tc, Map.add ctorOpt (varOpts, expr) exprMap, s)))))
                                (Ok(TCUncertain u, Map.empty, s)))
                        |> Result.map (fun (tc, exprMap, s) -> (Match(exprUnion, exprMap, tc, line), s)))))
            |> Result.mapError (atLine line)
        | VarRef(var, _, line) ->
            TypeCstrEnv.tryFind var tcenv
            |> Result.mapError TypeEnvError
            |> Result.map (fun tc -> (VarRef(var, tc, line), s))
            |> Result.mapError (atLine line)
        | Eq(t, expr1, expr2, _, line) ->
            if ClassEq.derivedBy um t then
                infer s tcenv expr1
                |> Result.bind (fun (expr1, s) ->
                    infer s tcenv expr2
                    |> Result.bind (fun (expr2, s) ->
                        unify s (get expr1) (get expr2)
                        |> Result.bind (fun (tcEq, s) ->
                            let tc, s, _ = generalize t s Map.empty in

                            unify s tcEq tc
                            |> Result.map (fun (_, s) -> (Eq(t, expr1, expr2, TCBool, line), s)))))
            else
                let tc, _, _ = generalize t s Map.empty in Error(TypeNotDerived(tc, ClassEq.name))
            |> Result.mapError (atLine line)
        | BoolNot(expr, _, line) ->
            infer s tcenv expr
            |> Result.bind (fun (expr, s) ->
                unify s (get expr) TCBool
                |> Result.map (fun (t, s) -> (BoolNot(expr, t, line), s)))
            |> Result.mapError (atLine line)
        | Less(t, expr1, expr2, _, line) ->
            if ClassOrd.derivedBy t then
                infer s tcenv expr1
                |> Result.bind (fun (expr1, s) ->
                    infer s tcenv expr2
                    |> Result.bind (fun (expr2, s) ->
                        unify s (get expr1) (get expr2)
                        |> Result.bind (fun (tcLess, s) ->
                            let tc, s, _ = generalize t s Map.empty in

                            unify s tcLess tc
                            |> Result.map (fun (_, s) -> (Less(t, expr1, expr2, TCBool, line), s)))))
            else
                let tc, _, _ = generalize t s Map.empty in Error(TypeNotDerived(tc, ClassOrd.name))
            |> Result.mapError (atLine line)
        | Plus(t, expr1, expr2, _, line) ->
            if ClassPlus.derivedBy t then
                infer s tcenv expr1
                |> Result.bind (fun (expr1, s) ->
                    infer s tcenv expr2
                    |> Result.bind (fun (expr2, s) ->
                        unify s (get expr1) (get expr2)
                        |> Result.bind (fun (tcPlus, s) ->
                            let tc, s, _ = generalize t s Map.empty in

                            unify s tcPlus tc
                            |> Result.map (fun (tcPlus, s) -> (Plus(t, expr1, expr2, tcPlus, line), s)))))
            else
                let t, _, _ = generalize t s Map.empty in Error(TypeNotDerived(t, ClassPlus.name))
            |> Result.mapError (atLine line)
        | Minus(t, expr1, expr2, _, line) ->
            if ClassMinus.derivedBy t then
                infer s tcenv expr1
                |> Result.bind (fun (expr1, s) ->
                    infer s tcenv expr2
                    |> Result.bind (fun (expr2, s) ->
                        unify s (get expr1) (get expr2)
                        |> Result.bind (fun (tcMinus, s) ->
                            let tc, s, _ = generalize t s Map.empty in

                            unify s tcMinus tc
                            |> Result.map (fun (tcMinus, s) -> (Minus(t, expr1, expr2, tcMinus, line), s)))))
            else
                let t, _, _ = generalize t s Map.empty in Error(TypeNotDerived(t, ClassMinus.name))
            |> Result.mapError (atLine line)
        | Times(t, expr1, expr2, _, line) ->
            if ClassTimes.derivedBy t then
                infer s tcenv expr1
                |> Result.bind (fun (expr1, s) ->
                    infer s tcenv expr2
                    |> Result.bind (fun (expr2, s) ->
                        unify s (get expr1) (get expr2)
                        |> Result.bind (fun (tcTimes, s) ->
                            let tc, s, _ = generalize t s Map.empty in

                            unify s tcTimes tc
                            |> Result.map (fun (tcTimes, s) -> (Times(t, expr1, expr2, tcTimes, line), s)))))
            else
                let t, _, _ = generalize t s Map.empty in Error(TypeNotDerived(t, ClassTimes.name))
            |> Result.mapError (atLine line)
        | Size(t, expr, _, line) ->
            if ClassSize.derivedBy t then
                infer s tcenv expr
                |> Result.bind (fun (expr, s) ->
                    let tc, s, _ = generalize t s Map.empty in

                    unify s tc (get expr)
                    |> Result.map (fun (_, s) -> (Size(t, expr, TCNat, line), s)))
            else
                let t, _, _ = generalize t s Map.empty in Error(TypeNotDerived(t, ClassSize.name))
            |> Result.mapError (atLine line)
        | Filter(t, var, expr1, expr2, _, line) ->
            if ClassEnum.derivedBy t then
                let tc, s, _ = generalize t s Map.empty

                let tcElem, s =
                    match tc with
                    | TCSet(tElem) -> (tElem, s)
                    | TCList(tElem) -> (tElem, s)
                    | TCMap(tK, _) -> (tK, s)
                    | TCUncertain _ -> let u, s = newUncertainVarId s in (TCUncertain u, s)
                    | _ -> failwith $"cannot get element type: %s{Type.format t}" in

                let tcenv = bind1 var tcElem tcenv in

                infer s tcenv expr1
                |> Result.bind (fun (expr1, s) ->
                    unify s (get expr1) TCBool
                    |> Result.bind (fun (_, s) ->
                        infer s tcenv expr2
                        |> Result.bind (fun (expr2, s) ->
                            unify s (get expr2) tc
                            |> Result.map (fun (tcFilter, s) -> (Filter(t, var, expr1, expr2, tcFilter, line), s)))))
            else
                let t, _, _ = generalize t s Map.empty in Error(atLine line (TypeNotDerived(t, ClassEnum.name)))
            |> Result.mapError (atLine line)
        | Exists(t, var, expr1, expr2, _, line) ->
            if ClassEnum.derivedBy t then
                let tc, s, _ = generalize t s Map.empty

                let tcElem, s =
                    match tc with
                    | TCSet(tElem) -> (tElem, s)
                    | TCList(tElem) -> (tElem, s)
                    | TCMap(tK, _) -> (tK, s)
                    | TCUncertain _ -> let u, s = newUncertainVarId s in (TCUncertain u, s)
                    | _ -> failwith $"cannot get element type: %s{Type.format t}" in

                let tcenv = bind1 var tcElem tcenv in

                infer s tcenv expr1
                |> Result.bind (fun (expr1, s) ->
                    unify s (get expr1) TCBool
                    |> Result.bind (fun (_, s) ->
                        infer s tcenv expr2
                        |> Result.bind (fun (expr2, s) ->
                            unify s (get expr2) tc
                            |> Result.map (fun (_, s) -> (Exists(t, var, expr1, expr2, TCBool, line), s)))))
            else
                let t, _, _ = generalize t s Map.empty in Error(atLine line (TypeNotDerived(t, ClassEnum.name)))
            |> Result.mapError (atLine line)
        | Contains(t, exprElem, exprList, _, line) ->
            if ClassEnum.derivedBy t then
                let tc, s, _ = generalize t s Map.empty

                let tcElem, s =
                    match tc with
                    | TCSet(tElem) -> (tElem, s)
                    | TCList(tElem) -> (tElem, s)
                    | TCMap(tK, _) -> (tK, s)
                    | TCUncertain _ -> let u, s = newUncertainVarId s in (TCUncertain u, s)
                    | _ -> failwith $"cannot get element type: %s{Type.format t}" in

                infer s tcenv exprElem
                |> Result.bind (fun (exprElem, s) ->
                    unify s (get exprElem) tcElem
                    |> Result.bind (fun (_, s) ->
                        infer s tcenv exprList
                        |> Result.bind (fun (exprList, s) ->
                            unify s (get exprList) tc
                            |> Result.map (fun (_, s) -> Contains(t, exprElem, exprList, TCBool, line), s))))
            else
                let t, _, _ = generalize t s Map.empty in Error(atLine line (TypeNotDerived(t, ClassEnum.name)))
            |> Result.mapError (atLine line)
        | Tuple(expr1, expr2, _, line) ->
            infer s tcenv expr1
            |> Result.bind (fun (expr1, s) ->
                infer s tcenv expr2
                |> Result.map (fun (expr2, s) -> (Tuple(expr1, expr2, TCTuple(get expr1, get expr2), line), s)))
            |> Result.mapError (atLine line)
        | TupleFst(exprTuple, _, line) ->
            infer s tcenv exprTuple
            |> Result.bind (fun (exprTuple, s) ->
                let uL, s = newUncertainVarId s in
                let uR, s = newUncertainVarId s in

                unify s (get exprTuple) (TCTuple(TCUncertain uL, TCUncertain uR))
                |> Result.map (fun (tc, s) ->
                    match tc with
                    | TCTuple(tcL, _) -> (TupleFst(exprTuple, tcL, line), s)
                    | _ -> failwith $"unification between TTuple and any must return TTuple, but come: %A{tc}"))
            |> Result.mapError (atLine line)
        | TupleSnd(exprTuple, _, line) ->
            infer s tcenv exprTuple
            |> Result.bind (fun (exprTuple, s) ->
                let uL, s = newUncertainVarId s in
                let uR, s = newUncertainVarId s in

                unify s (get exprTuple) (TCTuple(TCUncertain uL, TCUncertain uR))
                |> Result.map (fun (tc, s) ->
                    match tc with
                    | TCTuple(_, tcR) -> (TupleSnd(exprTuple, tcR, line), s)
                    | _ -> failwith $"unification between TTuple and any must return TTuple, but come: %A{tc}"))
            |> Result.mapError (atLine line)
        | ListCons(exprElem, exprList, _, line) ->
            infer s tcenv exprElem
            |> Result.bind (fun (exprElem, s) ->
                infer s tcenv exprList
                |> Result.bind (fun (exprList, s) ->
                    unify s (get exprList) (TCList(get exprElem))
                    |> Result.map (fun (tcList, s) -> (ListCons(exprElem, exprList, tcList, line), s))))
            |> Result.mapError (atLine line)
        | ListNth(exprList, exprIdx, _, line) ->
            infer s tcenv exprList
            |> Result.bind (fun (exprList, s) ->
                let u, s = newUncertainVarId s in

                unify s (get exprList) (TCList(TCUncertain u))
                |> Result.bind (fun (tc, s) ->
                    match tc with
                    | TCList(tcElem) ->
                        infer s tcenv exprIdx
                        |> Result.bind (fun (exprIdx, s) ->
                            unify s (get exprIdx) TCNat
                            |> Result.map (fun (_, s) -> (ListNth(exprList, exprIdx, tcElem, line), s)))
                    | _ -> failwith $"unification between TList and any must return TList, but come: %A{tc}"))
            |> Result.mapError (atLine line)
        | SetRange(exprLower, exprUpper, _, line) ->
            infer s tcenv exprLower
            |> Result.bind (fun (exprLower, s) ->
                unify s (get exprLower) TCNat
                |> Result.bind (fun (_, s) ->
                    infer s tcenv exprUpper
                    |> Result.bind (fun (exprUpper, s) ->
                        unify s (get exprUpper) TCNat
                        |> Result.bind (fun (_, s) -> Ok(ListNth(exprLower, exprUpper, TCSet(TCNat), line), s)))))
            |> Result.mapError (atLine line)
        | SetInsert(exprElem, exprSet, _, line) ->
            infer s tcenv exprElem
            |> Result.bind (fun (exprElem, s) ->
                infer s tcenv exprSet
                |> Result.bind (fun (exprSet, s) ->
                    unify s (get exprSet) (TCSet(get exprElem))
                    |> Result.map (fun (tcSet, s) -> (SetInsert(exprElem, exprSet, tcSet, line), s))))
            |> Result.mapError (atLine line)
        | SetRemove(exprElem, exprSet, _, line) ->
            infer s tcenv exprElem
            |> Result.bind (fun (exprElem, s) ->
                infer s tcenv exprSet
                |> Result.bind (fun (exprSet, s) ->
                    unify s (get exprSet) (TCSet(get exprElem))
                    |> Result.map (fun (tcSet, s) -> (SetInsert(exprElem, exprSet, tcSet, line), s))))
            |> Result.mapError (atLine line)
        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            infer s tcenv exprKey
            |> Result.bind (fun (exprKey, s) ->
                infer s tcenv exprVal
                |> Result.bind (fun (exprVal, s) ->
                    infer s tcenv exprMap
                    |> Result.bind (fun (exprMap, s) ->
                        unify s (get exprMap) (TCMap(get exprKey, get exprVal))
                        |> Result.map (fun (tcMap, s) -> (MapAdd(exprKey, exprVal, exprMap, tcMap, line), s)))))
            |> Result.mapError (atLine line)
        | MapFindOpt(exprKey, exprMap, _, line) ->
            infer s tcenv exprKey
            |> Result.bind (fun (exprKey, s) ->
                infer s tcenv exprMap
                |> Result.bind (fun (exprMap, s) ->
                    let u, s = newUncertainVarId s in

                    unify s (get exprMap) (TCMap(get exprKey, TCUncertain u))
                    |> Result.map (fun (tc, s) ->
                        match tc with
                        | TCMap(_, tcVal) -> (MapFindOpt(exprKey, exprMap, TCUnion("option", [ tcVal ]), line), s)
                        | _ -> failwith $"unification between TMap and any must return TMap, but come: %A{tc}")))
            |> Result.mapError (atLine line)
        | Univ(t, _, line) -> let tc, s, _ = generalize t s Map.empty in Ok(Univ(t, TCSet tc, line), s)

    infer s0 tcenv expr

let resolve (s: State) (expr: Expr<TypeCstr>) : Result<Expr<TypeCstr>, TypeError> =
    let expr = map (get >> resolve s) expr in

    match error expr with
    | Some(terr) -> Error(terr)
    | None -> Ok(map (fun expr -> ResultEx.get format (get expr)) expr)

let instantiate (expr: Expr<TypeCstr>) : Expr<Type> = map (get >> instantiate) expr

let postProcess (res: Result<Expr<TypeCstr> * State, TypeError>) : Result<Expr<Type> * State, TypeError> =
    res
    |> Result.bind (fun (expr, s) -> resolve s expr |> Result.map (fun expr -> (instantiate expr, s)))
