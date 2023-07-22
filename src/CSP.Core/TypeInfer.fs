module CSP.Core.TypeInfer

open CSP.Core.Util
open CSP.Core.LineNum
open CSP.Core.Var
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.TypeEnv

type TClassName = string

type TypeError =
    | At of TypeError * string
    | TypeNotDerived of Type * TClassName
    | UnionNameMismatch of UnionName * UnionName
    | NotUnion of Type
    | NotTuple of Type
    | TupleIndexOutOfBounds of Type * uint
    | TupleLengthMismatch of Type list * Type list
    | TypeMismatch of Type * Type
    | NoSuchCtor of Ctor
    | CtorsMismatch of Set<Ctor> * Set<Ctor>
    | UnboundVariable of Var
    | UnionValueLenMismatch of Ctor * int * int
    | EmptyMatch

let atLine (err: TypeError) (line: LineNum) : TypeError = At(err, $"line %s{line}")

let rec unwrapTypeError (terr: TypeError) : TypeError =
    match terr with
    | At(terr, _) -> unwrapTypeError terr
    | _ -> terr

let formatTypeError (expr: Expr) (terr: TypeError) : string =
    let rec formatTypeError terr =
        match terr with
        | At(terr, hint) -> $"%s{formatTypeError terr}\n\tat %s{hint}"
        | TypeNotDerived(t, tcClassName) -> $"type not derived %s{tcClassName}: %s{format t}"
        | UnionNameMismatch(un1, un2) -> $"union name mismatch: %s{un1} vs %s{un2}"
        | TypeMismatch(t1, t2) -> $"type mismatch: %s{format t1} vs %s{format t2}"
        | NotUnion t -> $"not union: %s{format t}"
        | NotTuple t -> $"not tuple: %s{format t}"
        | TupleIndexOutOfBounds(t, idx) -> $"tuple index out of bounds: %s{format t} at %d{idx}"
        | TupleLengthMismatch(ts1, ts2) -> $"tuple length mismatch: %s{format (TTuple ts1)} vs %s{format (TTuple ts2)}"
        | UnionValueLenMismatch(ctor, actual, expected) ->
            $"length of associated values mismatch: %s{Ctor.format ctor} (got %d{actual}, want {expected})"
        | NoSuchCtor ctor -> $"no such data constructor: %s{Ctor.format ctor}"
        | UnboundVariable var -> $"unbound variable: %s{Var.format var}"
        | CtorsMismatch(s1, s2) ->
            let s1 = String.concat ", " (Seq.map Ctor.format s1) in
            let s2 = String.concat ", " (Seq.map Ctor.format s2) in
            $"constructors mismatch: {{%s{s1}}} vs {{%s{s2}}}"
        | EmptyMatch -> "match must be have at least one clause that including a default clause"

    $"""{Expr.format expr}
error: {formatTypeError terr}"""

let resolve (m: Map<TVarId, Type>) (u: TVarId) : Option<Type> =
    let rec resolve visited t =
        match t with
        | TVar u' ->
            if Set.contains u' visited then
                None
            else
                match Map.tryFind u' m with
                | Some t -> resolve (Set.add u' visited) t
                | None -> Some(TVar u')
        | TBool -> Some TBool
        | TNat -> Some TNat
        | TTuple(ts) ->
            Option.map
                TTuple
                (List.foldBack
                    (fun t tsOpt ->
                        match tsOpt, resolve visited t with
                        | Some ts, Some t -> Some(t :: ts)
                        | _ -> None)
                    ts
                    (Some []))
        | TUnion(un, cm) ->
            let cmOpt =
                Map.fold
                    (fun accOpt ctor ts ->
                        match accOpt, ListEx.allOrNothing (List.map (resolve visited) ts) with
                        | Some cm, Some ts -> Some(Map.add ctor ts cm)
                        | _ -> None)
                    (Some Map.empty)
                    cm in

            match cmOpt with
            | Some cm -> Some(TUnion(un, cm))
            | None -> None
        | TSet tcV -> Option.map TSet (resolve visited tcV)
        | TList tcV -> Option.map TSet (resolve visited tcV)
        | TMap(tcK, tcV) ->
            Option.bind (fun tcK -> Option.map (fun tcV -> TMap(tcK, tcV)) (resolve visited tcV)) (resolve visited tcK)

    resolve (Set[u]) (TVar u)

let unify (m: Map<TVarId, Type>) (t1: Type) (t2: Type) : Result<Type * Map<TVarId, Type>, TypeError> =
    let rec unify m t1 t2 =
        match t1, t2 with
        | TVar n, _ -> Ok(t2, Map.add n t2 m)
        | _, TVar n -> Ok(t1, Map.add n t1 m)
        | TBool, TBool -> Ok(TBool, m)
        | TNat, TNat -> Ok(TNat, m)
        | TTuple(ts1), TTuple(ts2) ->
            if List.length ts1 = List.length ts2 then
                Result.map
                    (fun (ts, m) -> (TTuple(ts), m))
                    (List.foldBack
                        (fun (i, t1, t2) ->
                            Result.bind
                                (fun (ts, m) ->
                                    match unify m t1 t2 with
                                    | Error terr ->
                                        Error(
                                            At(terr, $"the %d{i}th element of the tuple: {format t1} vs {format t2}")
                                        )
                                    | Ok(t, m) -> Ok(t :: ts, m)))
                        (List.mapi (fun i (t1, t2) -> (i, t1, t2)) (List.zip ts1 ts2))
                        (Ok([], m)))
            else
                Error(TupleLengthMismatch(ts1, ts2))
        | TUnion(un1, cm1), TUnion(un2, cm2) ->
            if un1 = un2 then
                let ctors1 = Set.ofSeq (Map.keys cm1) in
                let ctors2 = Set.ofSeq (Map.keys cm2) in
                let ctors = Set.union ctors1 ctors2 in

                let cmRes =
                    Set.fold
                        (fun accRes ctor ->
                            match accRes with
                            | Error terr -> Error terr
                            | Ok(cm, m) ->
                                match Map.tryFind ctor cm1, Map.tryFind ctor cm2 with
                                | Some ts1, Some ts2 ->
                                    Result.map
                                        (fun (ts, m) -> (Map.add ctor ts cm, m))
                                        (List.foldBack
                                            (fun (t1, t2) ->
                                                Result.bind
                                                    (fun (ts, m) ->
                                                        match unify m t1 t2 with
                                                        | Ok(t, m) -> Ok(t :: ts, m)
                                                        | Error terr ->
                                                            Error(At(terr, $"the type list of %s{Ctor.format ctor}"))))
                                            (List.zip ts1 ts2)
                                            (Ok([], m)))
                                | _ -> Error(CtorsMismatch(ctors1, ctors2)))
                        (Ok(Map.empty, m))
                        ctors

                match cmRes with
                | Ok(cm, m) -> Ok(TUnion(un1, cm), m)
                | Error terr -> Error terr
            else
                Error(UnionNameMismatch(un1, un2))
        | TList tcV1, TList tcV2 ->
            match unify m tcV1 tcV2 with
            | Error terr -> Error(At(terr, $"the element type of the list: {format t1} vs {format t2}"))
            | Ok(tcV, m) -> Ok(TList(tcV), m)
        | TSet tcV1, TSet tcV2 ->
            match unify m tcV1 tcV2 with
            | Error terr -> Error(At(terr, $"the element type of the set: {format t1} vs {format t2}"))
            | Ok(tcV, m) -> Ok(TSet(tcV), m)
        | TMap(tcK1, tcV1), TMap(tcK2, tcV2) ->
            // NOTE: Do not use Result.mapError. It make hard to read.
            match unify m tcK1 tcK2 with
            | Error terr -> Error(At(terr, $"the key type of the set: {format t1} vs {format t2}"))
            | Ok(tcK, m) ->
                match unify m tcV1 tcV2 with
                | Error terr -> Error(At(terr, $"the value type of the set: {format t1} vs {format t2}"))
                | Ok(tcV, m) -> Ok(TMap(tcK, tcV), m)
        | _, _ -> Error(TypeMismatch(t1, t2))

    unify m t1 t2

let infer
    (cm: CtorMap)
    (n: TVarId)
    (m: Map<TVarId, Type>)
    (tenv: TypeEnv)
    (expr: Expr)
    : Result<Expr * Map<TVarId, Type> * TVarId, TypeError> =
    let rec infer n m tenv expr =
        match expr with
        | LitTrue(_, line) -> Ok(LitTrue(Some TBool, line), m, n)
        | LitFalse(_, line) -> Ok(LitFalse(Some TBool, line), m, n)
        | LitNat(n, _, line) -> Ok(LitNat(n, Some TNat, line), m, n)
        | LitEmpty(t, _, line) ->
            match t with
            | TSet tElem -> Ok(LitEmpty(TSet(tElem), Some(TSet tElem), line), m, n)
            | TList tElem -> Ok(LitEmpty(TList(tElem), Some(TList tElem), line), m, n)
            | TMap(tK, tV) -> Ok(LitEmpty(TMap(tK, tV), Some(TMap(tK, tV)), line), m, n)
            | _ -> Error(atLine (TypeNotDerived(t, "Empty")) line)
        | Union(ctor, exprs, _, line) ->
            match Map.tryFind ctor cm with
            | None -> Error(NoSuchCtor ctor)
            | Some(un, cm) ->
                match Map.tryFind ctor cm with
                | None -> Error(NoSuchCtor ctor)
                | Some ts ->
                    if List.length ts = List.length exprs then
                        let t = TUnion(un, cm) in

                        let exprsRes =
                            List.foldBack
                                (fun (t, expr) exprsRes ->
                                    match exprsRes with
                                    | Error terr -> Error terr
                                    | Ok(exprs, m, n) ->
                                        match infer n m tenv expr with
                                        | Error terr -> Error(atLine terr line)
                                        | Ok(expr, m, n) ->
                                            let t' = toType expr in

                                            match unify m t t' with
                                            | Error terr -> Error(atLine terr line)
                                            | Ok(_, m) -> Ok(expr :: exprs, m, n))
                                (List.zip ts exprs)
                                (Ok([], m, n)) in

                        match exprsRes with
                        | Ok(exprs, m, n) -> Ok(Union(ctor, exprs, Some t, line), m, n)
                        | Error terr -> Error terr
                    else
                        Error(UnionValueLenMismatch(ctor, List.length exprs, List.length ts))

        | If(exprCond, exprThen, exprElse, _, line) ->
            match infer n m tenv exprCond with
            | Error terr -> Error(atLine terr line)
            | Ok(exprCond, m, n) ->
                let tcCond = toType exprCond in

                match unify m tcCond TBool with
                | Error terr -> Error(atLine terr line)
                | Ok(_, m) ->
                    match infer n m tenv exprThen with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprThen, m, n) ->
                        let tcThen = toType exprThen in

                        match infer n m tenv exprElse with
                        | Error terr -> Error(atLine terr line)
                        | Ok(exprElse, m, n) ->
                            let tcElse = toType exprElse in

                            match unify m tcThen tcElse with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcIf, m) -> Ok(If(exprCond, exprThen, exprElse, Some tcIf, line), m, n)
        | Match(exprUnion, exprMap, defExpr, _, line) ->
            match infer n m tenv exprUnion with
            | Error terr -> Error(atLine terr line)
            | Ok(exprUnion, m, n) ->
                let tcUnion = toType exprUnion in

                match tcUnion with
                | TUnion(_, cm) ->
                    let accRes =
                        Map.fold
                            (fun acc ctor (vars, expr) ->
                                match acc with
                                | Error s -> Error s
                                | Ok(t, exprMap, n, m) ->
                                    match Map.tryFind ctor cm with
                                    | None -> Error(atLine (NoSuchCtor ctor) line)
                                    | Some ts ->
                                        if List.length ts = List.length vars then
                                            let tenv =
                                                List.fold
                                                    (fun tenv (t, var) -> Map.add var t tenv)
                                                    tenv
                                                    (List.zip ts vars) in

                                            match infer n m tenv expr with
                                            | Error terr -> Error(atLine terr line)
                                            | Ok(expr, m, n) ->
                                                let t' = toType expr in

                                                match unify m t t' with
                                                | Error terr -> Error(atLine terr line)
                                                | Ok(t, m) -> Ok(t, Map.add ctor (vars, expr) exprMap, n, m)
                                        else
                                            Error(UnionValueLenMismatch(ctor, List.length vars, List.length ts)))
                            (Ok(TVar n, Map.empty, n + 1u, m))
                            exprMap in

                    match accRes with
                    | Error terr -> Error(atLine terr line)
                    | Ok(t, exprMap, n, m) ->
                        match defExpr with
                        | None ->
                            if Map.isEmpty exprMap then
                                Error EmptyMatch
                            else
                                Ok(Match(exprUnion, exprMap, None, Some t, line), m, n)
                        | Some(varOpt, expr) ->
                            let tenv =
                                match varOpt with
                                | Some var -> Map.add var tcUnion tenv
                                | None -> tenv in

                            match infer n m tenv expr with
                            | Error terr -> Error(atLine terr line)
                            | Ok(expr, m, n) ->
                                let t' = toType expr in

                                match unify m t t' with
                                | Error terr -> Error(atLine terr line)
                                | Ok(t, m) -> Ok(Match(exprUnion, exprMap, Some(varOpt, expr), Some t, line), m, n)
                | _ -> Error(atLine (NotUnion(tcUnion)) line)
        | VarRef(var, _, line) ->
            match Map.tryFind var tenv with
            | Some t -> Ok(VarRef(var, Some t, line), m, n)
            | None -> Error(atLine (UnboundVariable var) line)
        | Eq(t, expr1, expr2, _, line) ->
            if ClassEq.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = toType expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = toType expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcEq, m) ->
                            match unify m tcEq t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, m) -> Ok(Eq(t, expr1, expr2, Some TBool, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassEq.name)) line)
        | BoolNot(expr, _, line) ->
            match infer n m tenv expr with
            | Error terr -> Error(atLine terr line)
            | Ok(expr, m, n) ->
                let t = toType expr in

                match unify m t TBool with
                | Error terr -> Error(atLine terr line)
                | Ok(t, m) -> Ok(BoolNot(expr, Some t, line), m, n)
        | Less(t, expr1, expr2, _, line) ->
            if ClassOrd.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = toType expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = toType expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcLess, m) ->
                            match unify m tcLess t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, m) -> Ok(Less(t, expr1, expr2, Some TBool, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassOrd.name)) line)
        | Plus(t, expr1, expr2, _, line) ->
            if ClassPlus.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = toType expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = toType expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcPlus, m) ->
                            match unify m tcPlus t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcPlus, m) -> Ok(Plus(t, expr1, expr2, Some tcPlus, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassPlus.name)) line)
        | Minus(t, expr1, expr2, _, line) ->
            if ClassMinus.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = toType expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = toType expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcMinus, m) ->
                            match unify m tcMinus t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcMinus, m) -> Ok(Minus(t, expr1, expr2, Some tcMinus, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassMinus.name)) line)
        | Times(t, expr1, expr2, _, line) ->
            if ClassTimes.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = toType expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = toType expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcTimes, m) ->
                            match unify m tcTimes t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcTimes, m) -> Ok(Times(t, expr1, expr2, Some tcTimes, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassTimes.name)) line)
        | Size(t, expr, _, line) ->
            if ClassSize.derivedBy t then
                match infer n m tenv expr with
                | Error terr -> Error(atLine terr line)
                | Ok(expr, m, n) ->
                    let t' = toType expr in

                    match unify m t t' with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, m) -> Ok(Size(t, expr, Some TNat, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassSize.name)) line)
        | Filter(t, var, expr1, expr2, _, line) ->
            if ClassEnum.derivedBy t then
                let elemT =
                    match t with
                    | TSet tElem -> tElem
                    | TList tElem -> tElem
                    | TMap(tK, _) -> tK
                    | _ -> failwith $"cannot get element type: %s{format t}" in

                match infer n m (Map.add var elemT tenv) expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = toType expr1 in

                    match unify m t1 TBool with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, m) ->
                        match infer n m tenv expr2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(expr2, m, n) ->
                            let t2 = toType expr2 in

                            match unify m t2 t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcFilter, m) -> Ok(Filter(t, var, expr1, expr2, Some tcFilter, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassEnum.name)) line)
        | Exists(t, var, expr1, expr2, _, line) ->
            if ClassEnum.derivedBy t then
                let elemT =
                    match t with
                    | TSet tElem -> tElem
                    | TList tElem -> tElem
                    | TMap(tK, _) -> tK
                    | _ -> failwith $"cannot get element type: %s{format t}" in

                match infer n m (Map.add var elemT tenv) expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = toType expr1 in

                    match unify m t1 TBool with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, m) ->
                        match infer n m tenv expr2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(expr2, m, n) ->
                            let t2 = toType expr2 in

                            match unify m t2 t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, m) -> Ok(Exists(t, var, expr1, expr2, Some TBool, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassEnum.name)) line)
        | Tuple(exprs, _, line) ->
            let accRes =
                List.foldBack
                    (fun expr accRes ->
                        match accRes with
                        | Error terr -> Error terr
                        | Ok(exprs, m, n) ->
                            match infer n m tenv expr with
                            | Error terr -> Error terr
                            | Ok(expr, m, n) -> Ok(expr :: exprs, m, n))
                    exprs
                    (Ok([], m, n)) in

            match accRes with
            | Ok(exprs, m, n) -> Ok(Tuple(exprs, Some(TTuple(List.map toType exprs)), line), m, n)
            | Error terr -> Error(atLine terr line)
        | TupleNth(exprTuple, idx, _, line) ->
            match infer n m tenv exprTuple with
            | Error terr -> Error(atLine terr line)
            | Ok(exprTuple, m, n) ->
                let tcTuple = toType exprTuple in

                match tcTuple with
                | TTuple(ts) ->
                    match List.tryItem (Checked.int idx) ts with
                    | Some t -> Ok(TupleNth(exprTuple, idx, Some(t), line), m, n)
                    | None -> Error(At(TupleIndexOutOfBounds(tcTuple, idx), $"line %s{line}"))
                | _ -> Error(At(NotTuple(tcTuple), $"line %s{line}"))

        | ListCons(exprElem, exprList, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, m, n) ->
                let tcElem = toType exprElem in

                match infer n m tenv exprList with
                | Error terr -> Error(atLine terr line)
                | Ok(exprList, m, n) ->
                    let tcList = toType exprList in

                    match unify m tcList (TList tcElem) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(tcList, m) -> Ok(ListCons(exprElem, exprList, Some tcList, line), m, n)
        | ListNth(exprList, exprIdx, _, line) ->
            match infer n m tenv exprList with
            | Error terr -> Error(atLine terr line)
            | Ok(exprList, m, n) ->
                let tcList = toType exprList in

                match unify m tcList (TList(TVar n)) with
                | Error terr -> Error(atLine terr line)
                | Ok(TList(tcElem), m) ->
                    match infer (n + 1u) m tenv exprIdx with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprIdx, m, n) ->
                        let tcIdx = toType exprIdx in

                        match unify m tcIdx TNat with
                        | Error terr -> Error(atLine terr line)
                        | Ok(_, m) -> Ok(ListNth(exprList, exprIdx, Some tcElem, line), m, n)
                | x -> failwith $"unification between TList and any must return TList, but come: %A{x}"
        | SetRange(exprLower, exprUpper, _, line) ->
            match infer n m tenv exprLower with
            | Error terr -> Error(atLine terr line)
            | Ok(exprLower, m, n) ->
                let tcLower = toType exprLower in

                match unify m tcLower TNat with
                | Error terr -> Error(atLine terr line)
                | Ok(_, m) ->
                    match infer n m tenv exprUpper with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprUpper, m, n) ->
                        let tcUpper = toType exprUpper in

                        match unify m tcUpper TNat with
                        | Error terr -> Error(atLine terr line)
                        | Ok(_, m) -> Ok(ListNth(exprLower, exprUpper, Some(TSet(TNat)), line), m, n)

        | SetInsert(exprElem, exprSet, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, m, n) ->
                let tcElem = toType exprElem in

                match infer n m tenv exprSet with
                | Error terr -> Error(atLine terr line)
                | Ok(exprSet, m, n) ->
                    let tcSet = toType exprSet in

                    match unify m tcSet (TSet(tcElem)) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(tcSet, m) -> Ok(SetInsert(exprElem, exprSet, Some tcSet, line), m, n)

        | SetMem(exprElem, exprSet, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, m, n) ->
                let tcElem = toType exprElem in

                match infer n m tenv exprSet with
                | Error terr -> Error(atLine terr line)
                | Ok(exprSet, m, n) ->
                    let tcSet = toType exprSet in

                    match unify m tcSet (TSet(tcElem)) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, m) -> Ok(SetMem(exprElem, exprSet, Some TBool, line), m, n)

        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            match infer n m tenv exprKey with
            | Error terr -> Error(atLine terr line)
            | Ok(exprKey, m, n) ->
                let tcKey = toType exprKey in

                match infer n m tenv exprVal with
                | Error terr -> Error(atLine terr line)
                | Ok(exprVal, m, n) ->
                    let tcVal = toType exprVal in

                    match infer n m tenv exprMap with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprMap, m, n) ->
                        let tcMap = toType exprMap in

                        match unify m tcMap (TMap(tcKey, tcVal)) with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcMap, m) -> Ok(MapAdd(exprKey, exprVal, exprMap, Some tcMap, line), m, n)

        | MapFindOpt(exprKey, exprMap, _, line) ->
            match infer n m tenv exprKey with
            | Error terr -> Error(atLine terr line)
            | Ok(exprKey, m, n) ->
                let tcKey = toType exprKey in

                match infer n m tenv exprMap with
                | Error terr -> Error(atLine terr line)
                | Ok(exprMap, m, n) ->
                    let tcMap = toType exprMap in

                    match unify m tcMap (TMap(tcKey, TVar(n))) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(TMap(_, tVal), m) ->
                        Ok(
                            MapFindOpt(
                                exprKey,
                                exprMap,
                                Some(TUnion("option", Map [ (Ctor "Some", [ tVal ]); Ctor "None", [] ])),
                                line
                            ),
                            m,
                            n + 1u
                        )
                    | x -> failwith $"unification between TMap and any must return TMap, but come: %A{x}"
        | Univ(t, _, line) -> Ok(Univ(t, Some(TSet t), line), m, n)

    match infer n m tenv expr with
    | Error err -> Error err
    | Ok(expr, m, n) ->
        let expr =
            mapType
                (Option.bind
                     (fun t ->
                         match t with
                         | TVar n -> resolve m n
                         | _ -> Some t))
                expr in

        Ok(expr, m, n)
