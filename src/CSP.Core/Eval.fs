module CSP.Core.Eval

open CSP.Core
open CSP.Core.UnionMap
open CSP.Core.Util
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.Expr
open CSP.Core.EvalError
open CSP.Core.Univ
open CSP.Core.Val
open CSP.Core.TypeShorthand
open CSP.Core.ValTypeCheck

type EvalConfig = { UnivConfig: UnivConfig }
let evalConfig cfg = { UnivConfig = cfg }

let eval (cfg: EvalConfig) (um: UnionMap) (cm: CtorMap) (env: Env) (expr: Expr<'a>) : Result<Val, EvalError> =
    let univ = univ cfg.UnivConfig um in

    let typeCheck t v =
        if typeCheck um cm t v then
            Ok(v)
        else
            Error(TypeMismatch(v, t)) in

    let tryGetUnion v =
        match v with
        | VUnion(ctor, vs) -> Ok(ctor, vs)
        | _ -> Error(ValNotUnion(v)) in

    let tryGetBool v =
        match v with
        | VBool(b) -> Ok(b)
        | _ -> Error(TypeMismatch(v, tBool))

    let tryGetNat v =
        match v with
        | VNat(n) -> Ok(n)
        | _ -> Error(TypeMismatch(v, tNat))

    let tryGetTuple v =
        match v with
        | VTuple(vL, vR) -> Ok(vL, vR)
        | _ -> Error(TypeMismatch(v, tTuple2 (tVar 0u) (tVar 1u))) in

    let tryGetSet v =
        match v with
        | VSet(vs) -> Ok(vs)
        | _ -> Error(TypeMismatch(v, tList (tVar 0u))) in

    let tryGetList v =
        match v with
        | VList(vs) -> Ok(vs)
        | _ -> Error(TypeMismatch(v, tList (tVar 0u))) in

    let tryGetMap v =
        match v with
        | VMap(m) -> Ok(m)
        | _ -> Error(TypeMismatch(v, tMap (tVar 0u) (tVar 1u))) in

    let rec eval env expr =
        match expr with
        | LitUnit _ -> Ok(VUnit)
        | LitTrue _ -> Ok(VBool(true))
        | LitFalse _ -> Ok(VBool(false))
        | LitNat(n, _, _) -> Ok(VNat(n))
        | LitEmpty(t, _, line) ->
            if ClassEmpty.derivedBy t then
                Ok(ClassEmpty.empty t)
            else
                Error(TypeNotDerived(t, ClassEmpty.name))
            |> Result.mapError (atLine line)
        | VarRef(var, _, line) -> valOf var env |> Result.mapError EnvError |> Result.mapError (atLine line)
        | Tuple(expr1, expr2, _, line) ->
            (expr1, expr2)
            |> ResultEx.bind2 (eval env) (eval env)
            |> Result.map VTuple
            |> Result.mapError (atLine line)
        | Union(ctor, exprs, _, line) ->
            tryFindAssocLen ctor um cm
            |> Result.mapError CtorMapError
            |> Result.bind (fun len ->
                if List.length exprs = len then
                    List.foldBack
                        (fun expr vsRes ->
                            match vsRes, eval env expr with
                            | Ok(vs), Ok(v) -> Ok(v :: vs)
                            | Error(err), _ -> Error err
                            | _, Error(err) -> Error err)
                        exprs
                        (Ok([]))
                    |> Result.map (fun vs -> VUnion(ctor, vs))
                else
                    Error(UnionValuesLenMismatch(ctor, len, List.length exprs)))
            |> Result.mapError (atLine line)
        | If(e1, e2, e3, _, line) ->
            eval env e1
            |> Result.bind tryGetBool
            |> Result.bind (fun b -> if b then eval env e2 else eval env e3)
            |> Result.mapError (atLine line)
        | Match(exprUnion, exprMap, _, line) ->
            eval env exprUnion
            |> Result.bind tryGetUnion
            |> Result.bind (fun (ctor, vs) ->
                match Map.tryFind (Some ctor) exprMap with
                | Some(varOpts, e1) ->
                    if List.length varOpts = List.length vs then
                        let env = bindAllOpts (List.zip varOpts vs) env in eval env e1
                    else
                        Error(UnionValuesLenMismatch(ctor, List.length varOpts, List.length vs))
                | None ->
                    match Map.tryFind None exprMap with
                    | Some(varOpts, e2) ->
                        match List.length varOpts with
                        | 1 -> let env = bind1Opt (List.head varOpts) (VUnion(ctor, vs)) env in eval env e2
                        | _ -> Error(DefaultClauseArgumentLenMustBe1 varOpts)
                    | None -> Error(NoClauseMatched ctor))
            |> Result.mapError (atLine line)
        | Eq(t, expr1, expr2, _, line) ->
            if ClassEq.derivedBy um t then
                (expr1, expr2)
                |> ResultEx.bind2 (Result.bind (typeCheck t) << eval env) (Result.bind (typeCheck t) << eval env)
                |> Result.map (fun (v1, v2) -> ClassEq.eq v1 v2)
            else
                Error(TypeNotDerived(t, ClassEq.name))
            |> Result.mapError (atLine line)
        | Less(t, expr1, expr2, _, line) ->
            if ClassOrd.derivedBy t then
                (expr1, expr2)
                |> ResultEx.bind2 (Result.bind (typeCheck t) << eval env) (Result.bind (typeCheck t) << eval env)
                |> Result.map (fun (v1, v2) -> ClassOrd.less v1 v2)
            else
                Error(TypeNotDerived(t, ClassOrd.name))
            |> Result.mapError (atLine line)
        | Size(t, expr, _, line) ->
            if ClassSize.derivedBy t then
                eval env expr
                |> Result.bind (typeCheck t)
                |> Result.map (VNat << ClassSize.size)
            else
                Error(TypeNotDerived(t, ClassSize.name))
            |> Result.mapError (atLine line)
        | BoolNot(e, _, line) ->
            eval env e
            |> Result.bind tryGetBool
            |> Result.map (VBool << not)
            |> Result.mapError (atLine line)
        | Plus(t, expr1, expr2, _, line) ->
            if ClassPlus.derivedBy t then
                (expr1, expr2)
                |> ResultEx.bind2 (Result.bind (typeCheck t) << eval env) (Result.bind (typeCheck t) << eval env)
                |> Result.map (fun (v1, v2) -> ClassPlus.plus v1 v2)
            else
                Error(TypeNotDerived(t, ClassPlus.name))
            |> Result.mapError (atLine line)
        | Times(t, expr1, expr2, _, line) ->
            if ClassTimes.derivedBy t then
                (expr1, expr2)
                |> ResultEx.bind2 (Result.bind (typeCheck t) << eval env) (Result.bind (typeCheck t) << eval env)
                |> Result.map (fun (v1, v2) -> ClassTimes.times v1 v2)
            else
                Error(TypeNotDerived(t, ClassTimes.name))
            |> Result.mapError (atLine line)
        | Minus(t, expr1, expr2, _, line) ->
            if ClassMinus.derivedBy t then
                (expr1, expr2)
                |> ResultEx.bind2 (Result.bind (typeCheck t) << eval env) (Result.bind (typeCheck t) << eval env)
                |> Result.map (fun (v1, v2) -> ClassMinus.minus v1 v2)
            else
                Error(TypeNotDerived(t, ClassMinus.name))
            |> Result.mapError (atLine line)
        | TupleFst(expr, _, line) ->
            eval env expr
            |> Result.bind tryGetTuple
            |> Result.map fst
            |> Result.mapError (atLine line)
        | TupleSnd(expr, _, line) ->
            eval env expr
            |> Result.bind tryGetTuple
            |> Result.map snd
            |> Result.mapError (atLine line)
        | ListNth(exprList, exprIdx, _, line) ->
            (exprList, exprIdx)
            |> ResultEx.bind2 (Result.bind tryGetList << eval env) (Result.bind tryGetNat << eval env)
            |> Result.bind (fun (vs, idx) ->
                match List.tryItem (Checked.int idx) vs with
                | Some v -> Ok(v)
                | None -> Error(ListIndexOutOfRange(vs, idx)))
            |> Result.mapError (atLine line)
        | ListCons(exprElem, exprList, _, line) ->
            (exprList, exprElem)
            |> ResultEx.bind2 (Result.bind tryGetList << eval env) (eval env)
            |> Result.map (fun (vs, v) -> VList(v :: vs))
            |> Result.mapError (atLine line)
        | SetRange(expr1, expr2, _, line) ->
            (expr1, expr2)
            |> ResultEx.bind2 (Result.bind tryGetNat << eval env) (Result.bind tryGetNat << eval env)
            |> Result.map (fun (n1, n2) -> VSet(Set.map VNat (Range.ofSet n1 n2)))
            |> Result.mapError (atLine line)
        | SetInsert(exprElem, exprSet, _, line) ->
            (exprElem, exprSet)
            |> ResultEx.bind2 (eval env) (Result.bind tryGetSet << eval env)
            |> Result.map (fun (v, s) -> Set.add v s)
            |> Result.map VSet
            |> Result.mapError (atLine line)
        | SetRemove(exprElem, exprSet, _, line) ->
            (exprElem, exprSet)
            |> ResultEx.bind2 (eval env) (Result.bind tryGetSet << eval env)
            |> Result.map (fun (v, s) -> Set.remove v s)
            |> Result.map VSet
            |> Result.mapError (atLine line)
        | Filter(t, var, e1, e2, _, line) ->
            if ClassEnum.derivedBy t then
                eval env e2
                |> Result.bind (fun v ->
                    match v with
                    | VSet s ->
                        s
                        |> Set.fold
                            (fun accRes v ->
                                accRes
                                |> Result.bind (fun sAcc ->
                                    eval (bind1 var v env) e1
                                    |> Result.bind tryGetBool
                                    |> Result.map (fun b -> if b then Set.add v sAcc else sAcc)))
                            (Ok Set.empty)
                        |> Result.map VSet
                    | VList vs ->
                        List.foldBack
                            (fun v accRes ->
                                accRes
                                |> Result.bind (fun vsAcc ->
                                    eval (bind1 var v env) e1
                                    |> Result.bind tryGetBool
                                    |> Result.map (fun b -> if b then v :: vsAcc else vsAcc)))
                            vs
                            (Ok([]))
                        |> Result.map VList
                    | VMap m ->
                        (Map.fold
                            (fun accRes k v ->
                                accRes
                                |> Result.bind (fun mAcc ->
                                    eval (bind1 var k env) e1
                                    |> Result.bind tryGetBool
                                    |> Result.map (fun b -> if b then Map.add k v mAcc else mAcc)))
                            (Ok(Map.empty))
                            m)
                        |> Result.map VMap
                    | _ -> failwith $"cannot filter: %s{format v}")
            else
                Error(TypeNotDerived(t, ClassEnum.name))
            |> Result.mapError (atLine line)
        | Exists(t, var, e1, e2, _, line) ->
            if ClassEnum.derivedBy t then
                eval env e2
                |> Result.bind (fun v ->
                    match v with
                    | VSet s ->
                        Set.fold
                            (fun accRes v ->
                                accRes
                                |> Result.bind (fun bAcc ->
                                    eval (bind1 var v env) e1 |> Result.bind tryGetBool |> Result.map ((||) bAcc)))
                            (Ok false)
                            s
                    | VList vs ->
                        List.foldBack
                            (fun v ->
                                Result.bind (fun bAcc ->
                                    eval (bind1 var v env) e1 |> Result.bind tryGetBool |> Result.map ((||) bAcc)))
                            vs
                            (Ok(false))
                    | VMap m ->
                        Map.fold
                            (fun accRes k _ ->
                                accRes
                                |> Result.bind (fun bAcc ->
                                    eval (bind1 var k env) e1 |> Result.bind tryGetBool |> Result.map ((||) bAcc)))
                            (Ok(false))
                            m
                    | _ -> failwith $"cannot satisfy Enum: %s{format v}")
                |> Result.map VBool
            else
                Error(TypeNotDerived(t, ClassEnum.name))
            |> Result.mapError (atLine line)
        | Contains(t, exprElem, exprList, _, line) ->
            if ClassEnum.derivedBy t then
                (exprList, exprElem)
                |> ResultEx.bind2 (eval env) (eval env)
                |> Result.map (fun (v1, v2) ->
                    match v2 with
                    | VSet(s) -> VBool(Set.contains v1 s)
                    | VList(vs) -> VBool(List.contains v1 vs)
                    | VMap(m) -> VBool(Map.containsKey v1 m)
                    | _ -> failwith $"cannot satisfy Enum: %s{format v2}")
            else
                Error(TypeNotDerived(t, ClassEnum.name))
            |> Result.mapError (atLine line)
        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            (exprKey, exprVal, exprMap)
            |> ResultEx.bind3 (eval env) (eval env) ((Result.bind tryGetMap) << (eval env))
            |> Result.map (fun (vK, vV, m) -> VMap(Map.add vK vV m))
            |> Result.mapError (atLine line)
        | MapFindOpt(exprKey, exprMap, _, line) ->
            (exprKey, exprMap)
            |> ResultEx.bind2 (eval env) ((Result.bind tryGetMap) << (eval env))
            |> Result.map (fun (vK, m) ->
                match Map.tryFind vK m with
                | Some v -> VUnion(Ctor "Some", [ v ])
                | None -> VUnion(Ctor "None", []))
            |> Result.mapError (atLine line)
        | Univ(t, _, line) ->
            univ t
            |> Result.map (Set.ofList >> VSet)
            |> Result.mapError (UnivError >> (atLine line))

    eval env expr
