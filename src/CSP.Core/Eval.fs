module CSP.Core.Eval

open CSP.Core
open CSP.Core.Util
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.Expr
open CSP.Core.EvalError
open CSP.Core.Univ
open CSP.Core.Val
open CSP.Core.Type
open CSP.Core.TypeShorthand

let rec chkType (t: Type) (v: Val) : bool =
    match t, v with
    | TUnit _, VUnit -> true
    | TVar _, _ -> true
    | TNat _, VNat _ -> true
    | TBool _, VBool _ -> true
    | TTuple(tL, tR), VTuple(vL, vR) -> chkType tL vL && chkType tR vR
    | TSet(tElem), VSet s -> Set.forall (chkType tElem) s
    | TList(tElem), VList vs -> List.forall (chkType tElem) vs
    | TMap(tK, tV), VMap m -> Map.forall (fun k v -> chkType tK k && chkType tV v) m
    | TUnion(_, cm), VUnion(ctor, vs) ->
        match Map.tryFind ctor cm with
        | Some ts -> List.length ts = List.length vs && List.forall2 chkType ts vs
        | None -> false
    | _, _ -> false

type EvalConfig = { UnivConfig: UnivConfig }
let evalConfig cfg = { UnivConfig = cfg }

let eval (cfg: EvalConfig) (cm: CtorMap) (env: Env) (expr: Expr<'a>) : Result<Val, EvalError> =
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
                Error(atLine line (TypeNotDerived(t, ClassEmpty.name)))
        | VarRef(var, _, line) -> Result.mapError (fun err -> atLine line (EnvError err)) (valOf var env)
        | Tuple(expr1, expr2, _, line) ->
            Result.mapError
                (atLine line)
                (Result.bind (fun v1 -> Result.map (fun v2 -> VTuple(v1, v2)) (eval env expr2)) (eval env expr1))

        | Union(ctor, exprs, _, line) ->
            match Map.tryFind ctor cm with
            | Some(_, cm') ->
                match Map.tryFind ctor cm' with
                | Some ts when List.length ts = List.length exprs ->
                    let vsRes =
                        List.foldBack
                            (fun expr vsRes ->
                                match vsRes, eval env expr with
                                | Ok(vs), Ok(v) -> Ok(v :: vs)
                                | Error(err), _ -> Error err
                                | _, Error(err) -> Error err)
                            exprs
                            (Ok([]))

                    match vsRes with
                    | Ok(vs) -> Ok(VUnion(ctor, vs))
                    | Error(err) -> Error(atLine line err)

                | Some ts -> Error(atLine line (UnionValuesLenMismatch(ctor, List.length exprs, List.length ts)))
                | None -> Error(atLine line (NoSuchCtor ctor))
            | None -> Error(atLine line (NoSuchCtor ctor))
        | If(e1, e2, e3, _, line) ->
            match eval env e1 with
            | Ok(VBool true) -> Result.mapError (fun err -> atLine line err) (eval env e2)
            | Ok(VBool false) -> Result.mapError (fun err -> atLine line err) (eval env e3)
            | Ok(v) -> Error(atLine line (ValNotBool v))
            | Error err -> Error(atLine line err)
        | Match(exprUnion, exprMap, _, line) ->
            match eval env exprUnion with
            | Ok(VUnion(ctor, vs)) ->
                match Map.tryFind (Some ctor) exprMap with
                | Some(varOpts, e1) ->
                    if List.length varOpts = List.length vs then
                        let env = bindAllOpts (List.zip varOpts vs) env in Result.mapError (atLine line) (eval env e1)
                    else
                        Error(atLine line (UnionValuesLenMismatch(ctor, List.length varOpts, List.length vs)))
                | None ->
                    match Map.tryFind None exprMap with
                    | Some(varOpts, e2) ->
                        match List.length varOpts with
                        | 1 -> let env = bind1Opt (List.head varOpts) (VUnion(ctor, vs)) env in eval env e2
                        | _ -> Error(atLine line (DefaultClauseArgumentLenMustBe1 varOpts))
                    | None -> Error(atLine line (NoClauseMatched ctor))
            | Ok(v) -> Error(atLine line (ValNotUnion v))
            | Error err -> Error(atLine line err)
        | Eq(t, e1, e2, _, line) ->
            if ClassEq.derivedBy t then
                match eval env e1 with
                | Error err -> Error(atLine line err)
                | Ok(v1) ->
                    if chkType t v1 then
                        match eval env e2 with
                        | Error err -> Error(atLine line err)
                        | Ok(v2) ->
                            if chkType t v2 then
                                Ok(VBool(ClassEq.eq v1 v2))
                            else
                                Error(atLine line (TypeMismatch(v2, t)))
                    else
                        Error(atLine line (TypeMismatch(v1, t)))
            else
                Error(atLine line (TypeNotDerived(t, ClassEq.name)))
        | Less(t, e1, e2, _, line) ->
            if ClassOrd.derivedBy t then
                match eval env e1 with
                | Error err -> Error(atLine line err)
                | Ok(v1) ->
                    if chkType t v1 then
                        match eval env e2 with
                        | Error err -> Error(atLine line err)
                        | Ok(v2) ->
                            if chkType t v2 then
                                Ok(VBool(ClassOrd.less v1 v2))
                            else
                                Error(atLine line (TypeMismatch(v2, t)))
                    else
                        Error(atLine line (TypeMismatch(v1, t)))
            else
                Error(atLine line (TypeNotDerived(t, ClassOrd.name)))
        | Size(t, expr, _, line) ->
            if ClassSize.derivedBy t then
                match eval env expr with
                | Ok v -> Ok(VNat(ClassSize.size v))
                | Error err -> Error(atLine line err)
            else
                Error(atLine line (TypeNotDerived(t, ClassSize.name)))
        | BoolNot(e, _, line) ->
            match eval env e with
            | Ok(VBool b) -> Ok(VBool(not b))
            | Ok(v) -> Error(atLine line (TypeMismatch(v, tBool)))
            | Error err -> Error(atLine line err)
        | Plus(t, e1, e2, _, line) ->
            if ClassPlus.derivedBy t then
                match eval env e1 with
                | Ok v1 ->
                    if chkType t v1 then
                        match eval env e2 with
                        | Ok v2 ->
                            if chkType t v2 then
                                Ok(ClassPlus.plus v1 v2)
                            else
                                Error(TypeMismatch(v2, t))
                        | Error err -> Error(atLine line err)
                    else
                        Error(TypeMismatch(v1, t))
                | Error err -> Error(atLine line err)
            else
                Error(atLine line (TypeNotDerived(t, ClassPlus.name)))
        | Times(t, e1, e2, _, line) ->
            if ClassTimes.derivedBy t then
                match eval env e1 with
                | Ok v1 ->
                    if chkType t v1 then
                        match eval env e2 with
                        | Ok v2 ->
                            if chkType t v2 then
                                Ok(ClassTimes.times v1 v2)
                            else
                                Error(TypeMismatch(v2, t))
                        | Error err -> Error(atLine line err)
                    else
                        Error(TypeMismatch(v1, t))
                | Error err -> Error(atLine line err)
            else
                Error(atLine line (TypeNotDerived(t, ClassTimes.name)))
        | Minus(t, e1, e2, _, line) ->
            if ClassMinus.derivedBy t then
                match eval env e1 with
                | Ok v1 ->
                    if chkType t v1 then
                        match eval env e2 with
                        | Ok v2 ->
                            if chkType t v2 then
                                Ok(ClassMinus.minus v1 v2)
                            else
                                Error(TypeMismatch(v2, t))
                        | Error err -> Error(atLine line err)
                    else
                        Error(TypeMismatch(v1, t))
                | Error err -> Error(atLine line err)
            else
                Error(atLine line (TypeNotDerived(t, ClassMinus.name)))
        | TupleFst(expr, _, line) ->
            match eval env expr with
            | Ok(VTuple(vL, _)) -> Ok(vL)
            | Ok(v) -> Error(atLine line (ValNotTuple(v)))
            | Error err -> Error(atLine line err)
        | TupleSnd(expr, _, line) ->
            match eval env expr with
            | Ok(VTuple(_, vR)) -> Ok(vR)
            | Ok(v) -> Error(atLine line (ValNotTuple(v)))
            | Error err -> Error(atLine line err)
        | ListNth(exprList, exprIdx, _, line) ->
            match eval env exprList with
            | Ok(VList vs) ->
                match eval env exprIdx with
                | Ok(VNat idx) ->
                    match List.tryItem (Checked.int idx) vs with
                    | Some v -> Ok v
                    | None -> Error(atLine line (ListIndexOutOfRange(VList vs, idx)))
                | Ok(v) -> Error(atLine line (TypeMismatch(v, tNat)))
                | Error err -> Error(atLine line err)
            | Ok(v) -> Error(atLine line (TypeMismatch(v, tList (tVar 0u))))
            | Error err -> Error(atLine line err)
        | ListCons(exprElem, exprList, _, line) ->
            match eval env exprList with
            | Ok(VList vs) ->
                match eval env exprElem with
                | Ok(v) -> Ok(VList(v :: vs))
                | Error err -> Error(atLine line err)
            | Ok(v) -> Error(atLine line (TypeMismatch(v, tList (tVar 0u))))
            | Error err -> Error(atLine line err)
        | SetRange(e1, e2, _, line) ->
            match eval env e1 with
            | Ok(VNat n1) ->
                match eval env e2 with
                | Ok(VNat n2) -> Ok(VSet(Set.map VNat (Range.ofSet n1 n2)))
                | Ok(v) -> Error(atLine line (TypeMismatch(v, tNat)))
                | Error err -> Error(atLine line err)
            | Ok(v) -> Error(atLine line (TypeMismatch(v, tNat)))
            | Error err -> Error(atLine line err)
        | SetInsert(exprElem, exprSet, _, line) ->
            match eval env exprSet with
            | Ok(VSet s) ->
                match eval env exprElem with
                | Ok(v) -> Ok(VSet(Set.add v s))
                | Error err -> Error(atLine line err)
            | Ok(v) -> Error(atLine line (TypeMismatch(v, tSet (tVar 0u))))
            | Error err -> Error(atLine line err)
        | SetRemove(exprElem, exprSet, _, line) ->
            match eval env exprSet with
            | Ok(VSet s) ->
                match eval env exprElem with
                | Ok(v) -> Ok(VSet(Set.remove v s))
                | Error err -> Error(atLine line err)
            | Ok(v) -> Error(atLine line (TypeMismatch(v, tSet (tVar 0u))))
            | Error err -> Error(atLine line err)
        | SetMem(exprElem, exprSet, _, line) ->
            match eval env exprSet with
            | Ok(VSet s) ->
                match eval env exprElem with
                | Ok(v) -> Ok(VBool(Set.contains v s))
                | Error err -> Error(atLine line err)
            | Ok(v) -> Error(atLine line (TypeMismatch(v, tSet (tVar 0u))))
            | Error err -> Error(atLine line err)
        | Filter(t, var, e1, e2, _, line) ->
            if ClassEnum.derivedBy t then
                match eval env e2 with
                | Ok(VSet s) ->
                    let sRes =
                        (Set.fold
                            (fun accRes v ->
                                match accRes with
                                | Ok sAcc ->
                                    let env = bind1 var v env in

                                    match eval env e1 with
                                    | Ok(VBool b) -> Ok(if b then Set.add v sAcc else sAcc)
                                    | Ok(v) -> Error(TypeMismatch(v, tBool))
                                    | Error err -> Error err
                                | Error err -> Error err)
                            (Ok Set.empty)
                            s) in

                    match sRes with
                    | Ok s -> Ok(VSet s)
                    | Error err -> Error(atLine line err)
                | Ok(VList vs) ->
                    let vsRes =
                        List.foldBack
                            (fun v accRes ->
                                match accRes with
                                | Ok(vsAcc) ->
                                    let env = bind1 var v env in

                                    match eval env e1 with
                                    | Ok(VBool b) -> Ok(if b then v :: vsAcc else vsAcc)
                                    | Ok(v) -> Error(TypeMismatch(v, tBool))
                                    | Error err -> Error err
                                | Error err -> Error err)
                            vs
                            (Ok([])) in

                    match vsRes with
                    | Ok vs -> Ok(VList vs)
                    | Error err -> Error(atLine line err)
                | Ok(VMap m) ->
                    let mAcc =
                        (Map.fold
                            (fun accRes k v ->
                                match accRes with
                                | Ok(mAcc) ->
                                    let env = bind1 var k env in

                                    match eval env e1 with
                                    | Ok(VBool b) -> Ok(if b then Map.add k v mAcc else mAcc)
                                    | Ok(v) -> Error(TypeMismatch(v, tBool))
                                    | Error err -> Error err
                                | Error err -> Error err)
                            (Ok(Map.empty))
                            m) in

                    match mAcc with
                    | Ok m -> Ok(VMap m)
                    | Error err -> Error(atLine line err)
                | Ok(v) -> failwith $"cannot filter: %s{Val.format v}"
                | Error err -> Error(atLine line err)
            else
                Error(atLine line (TypeNotDerived(t, ClassEnum.name)))
        | Exists(t, var, e1, e2, _, line) ->
            if ClassEnum.derivedBy t then
                match eval env e2 with
                | Ok(VSet s) ->
                    let bRes =
                        (Set.fold
                            (fun accRes v ->
                                match accRes with
                                | Ok bAcc ->
                                    let env = bind1 var v env in

                                    match eval env e1 with
                                    | Ok(VBool b) -> Ok(b || bAcc)
                                    | Ok(v) -> Error(TypeMismatch(v, tBool))
                                    | Error err -> Error err
                                | Error err -> Error err)
                            (Ok false)
                            s) in

                    match bRes with
                    | Ok b -> Ok(VBool b)
                    | Error err -> Error(atLine line err)
                | Ok(VList vs) ->
                    let bRes =
                        List.foldBack
                            (fun v accRes ->
                                match accRes with
                                | Ok(bAcc) ->
                                    let env = bind1 var v env in

                                    match eval env e1 with
                                    | Ok(VBool b) -> Ok(b || bAcc)
                                    | Ok(v) -> Error(TypeMismatch(v, tBool))
                                    | Error err -> Error err
                                | Error err -> Error err)
                            vs
                            (Ok(false)) in

                    match bRes with
                    | Ok b -> Ok(VBool b)
                    | Error err -> Error(atLine line err)
                | Ok(VMap m) ->
                    let bRes =
                        Map.fold
                            (fun accRes k _ ->
                                match accRes with
                                | Ok(bAcc) ->
                                    let env = bind1 var k env in

                                    match eval env e1 with
                                    | Ok(VBool b) -> Ok(b || bAcc)
                                    | Ok(v) -> Error(TypeMismatch(v, tBool))
                                    | Error err -> Error err
                                | Error err -> Error err)
                            (Ok(false))
                            m in

                    match bRes with
                    | Ok b -> Ok(VBool b)
                    | Error err -> Error(atLine line err)
                | Ok(v) -> failwith $"cannot satisfy exists: %s{Val.format v}"
                | Error err -> Error(atLine line err)
            else
                Error(atLine line (TypeNotDerived(t, ClassEnum.name)))
        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            match eval env exprKey with
            | Ok(vK) ->
                match eval env exprVal with
                | Ok(vV) ->
                    match eval env exprMap with
                    | Ok(VMap m) -> Ok(VMap(Map.add vK vV m))
                    | Ok(v) -> Error(atLine line (TypeMismatch(v, tMap (tVar 0u) (tVar 1u))))
                    | Error err -> Error(atLine line err)
                | Error err -> Error(atLine line err)
            | Error err -> Error(atLine line err)
        | MapFindOpt(exprKey, exprMap, _, line) ->
            match eval env exprKey with
            | Ok(vK) ->
                match eval env exprMap with
                | Ok(VMap m) ->
                    match Map.tryFind vK m with
                    | Some v -> Ok(VUnion(Ctor "Some", [ v ]))
                    | None -> Ok(VUnion(Ctor "None", []))
                | Ok(v) -> Error(atLine line (TypeMismatch(v, tMap (tVar 0u) (tVar 1u))))
                | Error err -> Error(atLine line err)
            | Error err -> Error(atLine line err)
        | Univ(t, _, line) ->
            match univ cfg.UnivConfig t with
            | Ok vs -> Ok(VSet(Set.ofList vs))
            | Error err -> Error(atLine line (UnivError err))

    eval env expr
