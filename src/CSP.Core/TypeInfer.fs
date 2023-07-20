module CSP.Core.TypeInfer

open CSP.Core.Var
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.TypeEnv

type TypeError =
    | At of TypeError * string
    | TypeNotDerived of Type * TClassName
    | UnionNameMismatch of UnionNameCstr * UnionNameCstr
    | TypeMismatch of TypeCstr * TypeCstr
    | NoSuchCtor of Ctor
    | UnboundVariable of Var
    | EmptyMatch

let rec unwrapTypeError (terr: TypeError) : TypeError =
    match terr with
    | At(terr, _) -> unwrapTypeError terr
    | _ -> terr

let formatTypeError (expr: Expr) (terr: TypeError) : string =
    let rec formatTypeError terr =
        match terr with
        | At(terr, hint) -> $"{formatTypeError terr}\n\tat {hint}"
        | TypeNotDerived(t, tcClassName) -> $"type not derived {tcClassName}: {Type.format t}"
        | UnionNameMismatch(un1, un2) -> $"union name mismatch: {formatUnionName un1} vs {formatUnionName un2}"
        | TypeMismatch(tc1, tc2) -> $"type mismatch: {format tc1} vs {format tc2}"
        | NoSuchCtor ctor -> $"no such data constructor: {Ctor.format ctor}"
        | UnboundVariable var -> $"unbound variable: {Var.format var}"
        | EmptyMatch -> "match must be have at least one clause that including a default clause"

    $"""{Expr.format expr}
error: {formatTypeError terr}"""

let resolve (m: Map<TCVarId, TypeCstr>) (u: TCVarId) : Option<TypeCstr> =
    let rec resolve visited tc =
        match tc with
        | TCVar u' ->
            if Set.contains u' visited then
                None
            else
                match Map.tryFind u' m with
                | Some tc -> resolve (Set.add u' visited) tc
                | None -> Some(TCVar u')
        | TCUnit -> Some TCUnit
        | TCBool -> Some TCBool
        | TCNat -> Some TCNat
        | TCTuple(tcL, tcR) ->
            Option.bind
                (fun tcL -> Option.map (fun tcR -> TCTuple(tcL, tcR)) (resolve visited tcR))
                (resolve visited tcL)
        | TCUnion(un, tcV) -> Option.map (fun tcV -> TCUnion(un, tcV)) (resolve visited tcV)
        | TCSet tcV -> Option.map (fun tcV -> TCSet(tcV)) (resolve visited tcV)
        | TCList tcV -> Option.map (fun tcV -> TCSet(tcV)) (resolve visited tcV)
        | TCMap(tcK, tcV) ->
            Option.bind (fun tcK -> Option.map (fun tcV -> TCMap(tcK, tcV)) (resolve visited tcV)) (resolve visited tcK)
        | TCError -> Some TCError

    resolve (Set[u]) (TCVar u)

let unify
    (m: Map<TCVarId, TypeCstr>)
    (t1: TypeCstr)
    (t2: TypeCstr)
    : Result<TypeCstr * Map<TCVarId, TypeCstr>, TypeError> =
    let rec unify m tc1 tc2 =
        match tc1, tc2 with
        | TCVar n, _ -> Ok(tc2, Map.add n tc2 m)
        | _, TCVar n -> Ok(tc1, Map.add n tc1 m)
        | TCUnit, TCUnit -> Ok(TCUnit, m)
        | TCBool, TCBool -> Ok(TCBool, m)
        | TCNat, TCNat -> Ok(TCNat, m)
        | TCError, TCError -> Ok(TCError, m)
        | TCTuple(tcL1, tcR1), TCTuple(tcL2, tcR2) ->
            // NOTE: Do not use Result.mapError. It make hard to read.
            match unify m tcL1 tcL2 with
            | Error terr -> Error(At(terr, $"the left-hand side of the tuple: {format tc1} vs {format tc2}"))
            | Ok(tL, m) ->
                match unify m tcR1 tcR2 with
                | Error terr -> Error(At(terr, $"the right-hand side of the tuple: {format tc1} vs {format tc2}"))
                | Ok(tR, m) -> Ok(TCTuple(tL, tR), m)
        | TCUnion(un1, tcV1), TCUnion(un2, tcV2) ->
            // NOTE: Do not use Result.mapError. It make hard to read.
            match unify m tcV1 tcV2 with
            | Error terr -> Error(At(terr, $"the value type of the union: {format tc1} vs {format tc2}"))
            | Ok(tcV, m) ->
                if un1 = UNAny then Ok(TCUnion(un2, tcV), m)
                else if un2 = UNAny then Ok(TCUnion(un1, tcV), m)
                else if un1 = un2 then Ok(TCUnion(un1, tcV), m)
                else Error(UnionNameMismatch(un1, un2))
        | TCList tcV1, TCList tcV2 ->
            match unify m tcV1 tcV2 with
            | Error terr -> Error(At(terr, $"the element type of the list: {format tc1} vs {format tc2}"))
            | Ok(tcV, m) -> Ok(TCList(tcV), m)
        | TCSet tcV1, TCSet tcV2 ->
            match unify m tcV1 tcV2 with
            | Error terr -> Error(At(terr, $"the element type of the set: {format tc1} vs {format tc2}"))
            | Ok(tcV, m) -> Ok(TCSet(tcV), m)
        | TCMap(tcK1, tcV1), TCMap(tcK2, tcV2) ->
            // NOTE: Do not use Result.mapError. It make hard to read.
            match unify m tcK1 tcK2 with
            | Error terr -> Error(At(terr, $"the key type of the set: {format tc1} vs {format tc2}"))
            | Ok(tcK, m) ->
                match unify m tcV1 tcV2 with
                | Error terr -> Error(At(terr, $"the value type of the set: {format tc1} vs {format tc2}"))
                | Ok(tcV, m) -> Ok(TCMap(tcK, tcV), m)
        | _, _ -> Error(TypeMismatch(tc1, tc2))

    unify m t1 t2

let infer
    (cm: CtorMap)
    (n: TCVarId)
    (m: Map<TCVarId, TypeCstr>)
    (tenv: TypeEnv)
    (expr: Expr)
    : Result<Expr * Map<TCVarId, TypeCstr> * TCVarId, TypeError> =
    let rec infer n m tenv expr =
        match expr with
        | LitUnit(_, line) -> Ok(LitUnit(Some TCUnit, line), m, n)
        | LitTrue(_, line) -> Ok(LitTrue(Some TCBool, line), m, n)
        | LitFalse(_, line) -> Ok(LitFalse(Some TCBool, line), m, n)
        | LitNat(n, _, line) -> Ok(LitNat(n, Some TCNat, line), m, n)
        | LitEmpty(t, _, line) ->
            match t with
            | TSet tElem -> Ok(LitEmpty(TSet(tElem), Some(TCSet(ofType tElem)), line), m, n)
            | TList tElem -> Ok(LitEmpty(TList(tElem), Some(TCList(ofType tElem)), line), m, n)
            | TMap(tK, tV) -> Ok(LitEmpty(TMap(tK, tV), Some(TCMap(ofType tK, ofType tV)), line), m, n)
            | _ -> Error(At(TypeNotDerived(t, "Empty"), $"line {line}"))
        | LitError(_, line) -> Ok(LitError(Some TCError, line), m, n)
        | Union(ctor, expr, _, line) ->
            match Map.tryFind ctor cm with
            | None -> Error(NoSuchCtor ctor)
            | Some(un, tV) ->
                let tcV = ofType tV in

                match infer n m tenv expr with
                | Ok(expr, m, n) ->
                    let tcV' = typeCstr expr in

                    match unify m tcV tcV' with
                    | Ok(tcV, m) -> Ok(Union(ctor, expr, Some(TCUnion(UNName un, tcV)), line), m, n)
                    | Error terr -> Error(At(terr, $"line {line}"))
                | Error terr -> Error(At(terr, $"line {line}"))
        | Throw(_, line) -> Ok(Throw(Some TCError, line), m, n)
        | If(exprCond, exprThen, exprElse, _, line) ->
            match infer n m tenv exprCond with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprCond, m, n) ->
                let tcCond = typeCstr exprCond in

                match unify m tcCond TCBool with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(_, m) ->
                    match infer n m tenv exprThen with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(exprThen, m, n) ->
                        let tcThen = typeCstr exprThen in

                        match infer n m tenv exprElse with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(exprElse, m, n) ->
                            let tcElse = typeCstr exprElse in

                            match unify m tcThen tcElse with
                            | Error terr -> Error(At(terr, $"line {line}"))
                            | Ok(tcIf, m) -> Ok(If(exprCond, exprThen, exprElse, Some tcIf, line), m, n)
        | Match(exprUnion, exprMap, defExpr, _, line) ->
            match infer n m tenv exprUnion with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprUnion, m, n) ->
                let tcUnion = typeCstr exprUnion in

                match unify m tcUnion (TCUnion(UNAny, TCVar n)) with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(_, m) ->
                    let n = n + 1u in

                    let accRes =
                        Map.fold
                            (fun acc ctor (varOpt, expr) ->
                                match acc with
                                | Error s -> Error s
                                | Ok(tc, exprMap, n, m) ->
                                    let tenvRes =
                                        match varOpt with
                                        | None -> Ok tenv
                                        | Some var ->
                                            match Map.tryFind ctor cm with
                                            | None -> Error(At(NoSuchCtor ctor, $"line {line}"))
                                            | Some(_, tV) -> Ok(Map.add var (ofType tV) tenv) in

                                    match tenvRes with
                                    | Error s -> Error s
                                    | Ok tenv ->
                                        match infer n m tenv expr with
                                        | Error terr -> Error(At(terr, $"line {line}"))
                                        | Ok(expr, m, n) ->
                                            let tc' = typeCstr expr in

                                            match unify m tc tc' with
                                            | Error terr -> Error(At(terr, $"line {line}"))
                                            | Ok(tc, m) -> Ok(tc, Map.add ctor (varOpt, expr) exprMap, n, m))
                            (Ok(TCVar n, Map.empty, n + 1u, m))
                            exprMap in

                    match accRes with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(tc, exprMap, n, m) ->
                        match defExpr with
                        | None ->
                            if Map.isEmpty exprMap then
                                Error EmptyMatch
                            else
                                Ok(Match(exprUnion, exprMap, None, Some tc, line), m, n)
                        | Some(varOpt, expr) ->
                            let tenv =
                                match varOpt with
                                | Some var -> Map.add var tcUnion tenv
                                | None -> tenv in

                            match infer n m tenv expr with
                            | Error terr -> Error(At(terr, $"line {line}"))
                            | Ok(expr, m, n) ->
                                let tc' = typeCstr expr in

                                match unify m tc tc' with
                                | Error terr -> Error(At(terr, $"line {line}"))
                                | Ok(tc, m) -> Ok(Match(exprUnion, exprMap, Some(varOpt, expr), Some tc, line), m, n)
        | VarRef(var, _, line) ->
            match Map.tryFind var tenv with
            | Some tc -> Ok(VarRef(var, Some tc, line), m, n)
            | None -> Error(At(UnboundVariable var, $"line {line}"))
        | Eq(t, expr1, expr2, _, line) ->
            match infer n m tenv expr1 with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(expr1, m, n) ->
                let tc1 = typeCstr expr1 in

                match infer n m tenv expr2 with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr2, m, n) ->
                    let tc2 = typeCstr expr2 in

                    match unify m tc1 tc2 with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(tcEq, m) ->
                        match unify m tcEq (ofType t) with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(_, m) -> Ok(Eq(t, expr1, expr2, Some TCBool, line), m, n)
        | Not(expr, _, line) ->
            match infer n m tenv expr with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(expr, m, n) ->
                let tc = typeCstr expr in

                match unify m tc TCBool with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(tc, m) -> Ok(Not(expr, Some tc, line), m, n)
        | Less(t, expr1, expr2, _, line) ->
            match infer n m tenv expr1 with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(expr1, m, n) ->
                let tc1 = typeCstr expr1 in

                match infer n m tenv expr2 with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr2, m, n) ->
                    let tc2 = typeCstr expr2 in

                    match unify m tc1 tc2 with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(tcLess, m) ->
                        match unify m tcLess (ofType t) with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(_, m) -> Ok(Less(t, expr1, expr2, Some TCBool, line), m, n)
        | Plus(t, expr1, expr2, _, line) ->
            let tRes =
                match t with
                | TBool -> Ok TBool
                | TNat -> Ok TBool
                | TSet(tElem) -> Ok(TSet tElem)
                | _ -> Error(TypeNotDerived(t, "Ring"))

            match tRes with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok _ ->
            match infer n m tenv expr1 with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(expr1, m, n) ->
                let tc1 = typeCstr expr1 in

                match infer n m tenv expr2 with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr2, m, n) ->
                    let tc2 = typeCstr expr2 in

                    match unify m tc1 tc2 with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(tcPlus, m) ->
                        match unify m tcPlus (ofType t) with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(tcPlus, m) -> Ok(Plus(t, expr1, expr2, Some tcPlus, line), m, n)
        | Minus(t, expr1, expr2, _, line) ->
            let tRes =
                match t with
                | TBool -> Ok TBool
                | TNat -> Ok TBool
                | TSet(tElem) -> Ok(TSet tElem)
                | _ -> Error(TypeNotDerived(t, "Field"))

            match tRes with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok _ ->
                match infer n m tenv expr1 with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr1, m, n) ->
                    let tc1 = typeCstr expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(expr2, m, n) ->
                        let tc2 = typeCstr expr2 in

                        match unify m tc1 tc2 with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(tcMinus, m) ->
                            match unify m tcMinus (ofType t) with
                            | Error terr -> Error(At(terr, $"line {line}"))
                            | Ok(tcMinus, m) -> Ok(Minus(t, expr1, expr2, Some tcMinus, line), m, n)
        | Times(t, expr1, expr2, _, line) ->
            let tRes =
                match t with
                | TBool -> Ok TBool
                | TNat -> Ok TBool
                | TSet(tElem) -> Ok(TSet tElem)
                | _ -> Error(TypeNotDerived(t, "Ring"))

            match tRes with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok _ ->
                match infer n m tenv expr1 with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr1, m, n) ->
                    let tc1 = typeCstr expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(expr2, m, n) ->
                        let tc2 = typeCstr expr2 in

                        match unify m tc1 tc2 with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(tcTimes, m) ->
                            match unify m tcTimes (ofType t) with
                            | Error terr -> Error(At(terr, $"line {line}"))
                            | Ok(tcTimes, m) -> Ok(Times(t, expr1, expr2, Some tcTimes, line), m, n)
        | Size(t, expr, _, line) ->
            let res =
                match t with
                | TSet _ -> Ok()
                | TList _ -> Ok()
                | TMap _ -> Ok()
                | _ -> Error(TypeNotDerived(t, "Size")) in

            match res with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok _ ->
                match infer n m tenv expr with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr, m, n) ->
                    let tc = typeCstr expr in

                    match unify m tc (ofType t) with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(_, m) -> Ok(Size(t, expr, Some TCNat, line), m, n)
        | Filter(t, var, expr1, expr2, _, line) ->
            let elemTRes =
                match t with
                | TSet elemT -> Ok elemT
                | TList elemT -> Ok elemT
                | _ -> Error(TypeNotDerived(t, "Enum")) in

            match elemTRes with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok elemT ->
                match infer n m (Map.add var (ofType elemT) tenv) expr1 with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr1, m, n) ->
                    let tc1 = typeCstr expr1 in

                    match unify m tc1 TCBool with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(_, m) ->
                        match infer n m tenv expr2 with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(expr2, m, n) ->
                            let tc2 = typeCstr expr2 in

                            match unify m tc2 (ofType t) with
                            | Error terr -> Error(At(terr, $"line {line}"))
                            | Ok(tcFilter, m) -> Ok(Filter(t, var, expr1, expr2, Some tcFilter, line), m, n)
        | Exists(t, var, expr1, expr2, _, line) ->
            let elemTRes =
                match t with
                | TSet elemT -> Ok elemT
                | TList elemT -> Ok elemT
                | _ -> Error(TypeNotDerived(t, "Enum")) in

            match elemTRes with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok elemT ->
                match infer n m (Map.add var (ofType elemT) tenv) expr1 with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr1, m, n) ->
                    let tc1 = typeCstr expr1 in

                    match unify m tc1 TCBool with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(_, m) ->
                        match infer n m tenv expr2 with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(expr2, m, n) ->
                            let tc2 = typeCstr expr2 in

                            match unify m tc2 (ofType t) with
                            | Error terr -> Error(At(terr, $"line {line}"))
                            | Ok(_, m) -> Ok(Exists(t, var, expr1, expr2, Some TCBool, line), m, n)
        | Tuple(expr1, expr2, _, line) ->
            match infer n m tenv expr1 with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(expr1, m, n) ->
                let tc1 = typeCstr expr1 in

                match infer n m tenv expr2 with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(expr2, m, n) ->
                    let tc2 = typeCstr expr2 in Ok(Tuple(expr1, expr2, Some(TCTuple(tc1, tc2)), line), m, n)
        | TupleFst(exprTuple, _, line) ->
            match infer n m tenv exprTuple with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprTuple, m, n) ->
                let tcTuple = typeCstr exprTuple in

                match unify m tcTuple (TCTuple(TCVar n, TCVar(n + 1u))) with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(TCTuple(tcLeft, _), m) -> Ok(TupleFst(exprTuple, Some tcLeft, line), m, n + 2u)
                | x -> failwith $"unification between TCTuple and any must return TCTuple, but: failed %A{x}"
        | TupleSnd(exprTuple, _, line) ->
            match infer n m tenv exprTuple with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprTuple, m, n) ->
                let tcTuple = typeCstr exprTuple in

                match unify m tcTuple (TCTuple(TCVar n, TCVar(n + 1u))) with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(TCTuple(_, tcRight), m) -> Ok(TupleFst(exprTuple, Some tcRight, line), m, n + 2u)
                | x -> failwith $"unification between TCTuple and any must return TCTuple, but come: %A{x}"
        | ListCons(exprElem, exprList, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprElem, m, n) ->
                let tcElem = typeCstr exprElem in

                match infer n m tenv exprList with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(exprList, m, n) ->
                    let tcList = typeCstr exprList in

                    match unify m tcList (TCList tcElem) with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(tcList, m) -> Ok(ListCons(exprElem, exprList, Some tcList, line), m, n)
        | ListNth(exprList, exprIdx, _, line) ->
            match infer n m tenv exprList with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprList, m, n) ->
                let tcList = typeCstr exprList in

                match unify m tcList (TCList(TCVar n)) with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(TCList(tcElem), m) ->
                    match infer (n + 1u) m tenv exprIdx with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(exprIdx, m, n) ->
                        let tcIdx = typeCstr exprIdx in

                        match unify m tcIdx TCNat with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(_, m) -> Ok(ListNth(exprList, exprIdx, Some tcElem, line), m, n)
                | x -> failwith $"unification between TCList and any must return TCList, but come: %A{x}"
        | SetRange(exprLower, exprUpper, _, line) ->
            match infer n m tenv exprLower with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprLower, m, n) ->
                let tcLower = typeCstr exprLower in

                match unify m tcLower TCNat with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(_, m) ->
                    match infer n m tenv exprUpper with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(exprUpper, m, n) ->
                        let tcUpper = typeCstr exprUpper in

                        match unify m tcUpper TCNat with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(_, m) -> Ok(ListNth(exprLower, exprUpper, Some(TCSet(TCNat)), line), m, n)

        | SetInsert(exprElem, exprSet, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprElem, m, n) ->
                let tcElem = typeCstr exprElem in

                match infer n m tenv exprSet with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(exprSet, m, n) ->
                    let tcSet = typeCstr exprSet in

                    match unify m tcSet (TCSet(tcElem)) with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(tcSet, m) -> Ok(SetInsert(exprElem, exprSet, Some tcSet, line), m, n)

        | SetMem(exprElem, exprSet, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprElem, m, n) ->
                let tcElem = typeCstr exprElem in

                match infer n m tenv exprSet with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(exprSet, m, n) ->
                    let tcSet = typeCstr exprSet in

                    match unify m tcSet (TCSet(tcElem)) with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(_, m) -> Ok(SetMem(exprElem, exprSet, Some TCBool, line), m, n)

        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            match infer n m tenv exprKey with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprKey, m, n) ->
                let tcKey = typeCstr exprKey in

                match infer n m tenv exprVal with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(exprVal, m, n) ->
                    let tcVal = typeCstr exprVal in

                    match infer n m tenv exprMap with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(exprMap, m, n) ->
                        let tcMap = typeCstr exprMap in

                        match unify m tcMap (TCMap(tcKey, tcVal)) with
                        | Error terr -> Error(At(terr, $"line {line}"))
                        | Ok(tcMap, m) -> Ok(MapAdd(exprKey, exprVal, exprMap, Some tcMap, line), m, n)

        | MapFindOpt(exprKey, exprMap, _, line) ->
            match infer n m tenv exprKey with
            | Error terr -> Error(At(terr, $"line {line}"))
            | Ok(exprKey, m, n) ->
                let tcKey = typeCstr exprKey in

                match infer n m tenv exprMap with
                | Error terr -> Error(At(terr, $"line {line}"))
                | Ok(exprMap, m, n) ->
                    let tcMap = typeCstr exprMap in

                    match unify m tcMap (TCMap(tcKey, TCVar(n))) with
                    | Error terr -> Error(At(terr, $"line {line}"))
                    | Ok(TCMap(_, tcVal), m) ->
                        Ok(MapFindOpt(exprKey, exprMap, Some(TCUnion(UNName "option", tcVal)), line), m, n + 1u)
                    | x -> failwith $"unification between TCMap and any must return TCMap, but come: %A{x}"
        | Univ(t, _, line) -> Ok(Univ(t, Some(TCSet(ofType t)), line), m, n)

    match infer n m tenv expr with
    | Error err -> Error err
    | Ok(expr, m, n) ->
        let expr =
            mapTypeCstr
                (fun tcOpt ->
                    Option.bind
                        (fun tc ->
                            match tc with
                            | TCVar n -> resolve m n
                            | _ -> Some tc)
                        tcOpt)
                expr in

        Ok(expr, m, n)
