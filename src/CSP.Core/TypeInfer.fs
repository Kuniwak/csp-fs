module CSP.Core.TypeInfer

open CSP.Core.LineNum
open CSP.Core.TypeCstr
open CSP.Core.Util
open CSP.Core.Var
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.TypeEnv

type TypeError =
    | At of TypeError * string
    | TypeNotDerived of Type * TypeClassName
    | UnionNameMismatch of UnionName * UnionName
    | NotUnion of Type
    | NotTuple of Type
    | TupleIndexOutOfBounds of Type * uint
    | TupleLengthMismatch of Set<TypeCstr list>
    | TypeMismatch of Set<TypeCstr>
    | NoSuchCtor of Ctor
    | CtorsMismatch of Set<Ctor> * Set<Ctor>
    | UnboundVariable of Var
    | UnionValueLenMismatch of Ctor * int * int
    | EmptyMatch
    | DefaultClauseArgumentsLenMustBe1 of Var option list
    | Recursion of UncertainVarId * TypeCstrUncertainVar.VarMap

let atLine (err: TypeError) (line: LineNum) : TypeError = At(err, $"line %s{line}")

let rec unwrapTypeError (terr: TypeError) : TypeError =
    match terr with
    | At(terr, _) -> unwrapTypeError terr
    | _ -> terr

let formatTypeError (terr: TypeError) : string =
    let rec formatTypeError terr =
        match terr with
        | At(terr, hint) -> $"%s{formatTypeError terr}\n\tat %s{hint}"
        | TypeNotDerived(t, tcClassName) -> $"type not derived %s{tcClassName}: %s{format t}"
        | UnionNameMismatch(un1, un2) -> $"union name mismatch: %s{un1} vs %s{un2}"
        | TypeMismatch(s) ->
            let s = String.concat " vs " (List.map TypeCstr.format (Set.toList s)) in $"type mismatch: %s{s}"
        | NotUnion t -> $"not union: %s{format t}"
        | NotTuple t -> $"not tuple: %s{format t}"
        | TupleIndexOutOfBounds(t, idx) -> $"tuple index out of bounds: %s{format t} at %d{idx}"
        | TupleLengthMismatch(s) ->
            let s =
                String.concat " vs " (List.map (fun tcs -> TypeCstr.format (TCTuple tcs)) (Set.toList s))

            $"tuple length mismatch: %s{s}"
        | UnionValueLenMismatch(ctor, actual, expected) ->
            $"length of associated values mismatch: %s{Ctor.format ctor} (got %d{actual}, want {expected})"
        | NoSuchCtor ctor -> $"no such data constructor: %s{Ctor.format ctor}"
        | UnboundVariable var -> $"unbound variable: %s{Var.format var}"
        | CtorsMismatch(s1, s2) ->
            let s1 = String.concat ", " (Seq.map Ctor.format s1) in
            let s2 = String.concat ", " (Seq.map Ctor.format s2) in
            $"constructors mismatch: {{%s{s1}}} vs {{%s{s2}}}"
        | EmptyMatch -> "match must be have at least one clause that including a default clause"
        | DefaultClauseArgumentsLenMustBe1(vars) ->
            let s =
                String.concat
                    ","
                    (List.map
                        (fun varOpt ->
                            match varOpt with
                            | Some var -> Var.format var
                            | None -> "_")
                        vars) in

            $"length of arguments for default clause must be 1, but got: [%s{s}]"
        | Recursion(u, m) ->
            let s =
                String.concat
                    "\n"
                    (List.map
                        (fun (u, t) -> $"  %s{TypeCstr.format (TCUncertain u)} -> %s{TypeCstr.format t}")
                        (Map.toList m.Map)) in

            $"type recursion: %s{TypeCstr.format (TCUncertain u)} in\n%s{s}"

    $"""type error: {formatTypeError terr}"""

type InferState =
    { UncertainVarMap: TypeCstrUncertainVar.VarMap }

let newUncertainVarId (s: InferState) : UncertainVarId * InferState =
    let id, fam = TypeCstrUncertainVar.newId s.UncertainVarMap in (id, { s with UncertainVarMap = fam })

let bindUncertainVar (id: UncertainVarId) (tc: TypeCstr) (s: InferState) : InferState =
    let fum = TypeCstrUncertainVar.bind id tc s.UncertainVarMap in { s with UncertainVarMap = fum }

let resolveUncertainVar (id: UncertainVarId) (s: InferState) : TypeCstr option =
    TypeCstrUncertainVar.resolve id s.UncertainVarMap

let resolve (s: InferState) (t: TypeCstr) : Result<TypeCstr, TypeError> =
    let rec resolve visited t =
        match t with
        | TCUncertain u ->
            if Set.contains u visited then
                Error(Recursion(u, s.UncertainVarMap))
            else
                match TypeCstrUncertainVar.resolve u s.UncertainVarMap with
                | Some t -> resolve (Set.add u visited) t
                | None -> Ok(TCUncertain u)
        | TCBool -> Ok TCBool
        | TCNat -> Ok TCNat
        | TCTuple(ts) ->
            Result.map
                TCTuple
                (List.foldBack
                    (fun t tsOpt ->
                        match tsOpt, resolve visited t with
                        | Ok ts, Ok t -> Ok(t :: ts)
                        | Error err, _ -> Error err
                        | _, Error err -> Error err)
                    ts
                    (Ok []))
        | TCUnion(un, cm) ->
            let cmRes =
                Map.fold
                    (fun accRes ctor ts ->
                        let tsRes =
                            List.foldBack
                                (fun t -> Result.bind (fun ts -> Result.map (fun t -> t :: ts) (resolve visited t)))
                                ts
                                (Ok([]))

                        match accRes, tsRes with
                        | Ok cm, Ok ts -> Ok(Map.add ctor ts cm)
                        | Error err, _ -> Error err
                        | _, Error err -> Error err)
                    (Ok Map.empty)
                    cm in

            Result.map (fun cm -> TCUnion(un, cm)) cmRes
        | TCSet tcV -> Result.map TCSet (resolve visited tcV)
        | TCList tcV -> Result.map TCList (resolve visited tcV)
        | TCMap(tcK, tcV) ->
            Result.bind (fun tcK -> Result.map (fun tcV -> TCMap(tcK, tcV)) (resolve visited tcV)) (resolve visited tcK)

    resolve (Set []) t

let unify (s: InferState) (t1: TypeCstr) (t2: TypeCstr) : Result<Type * InferState, TypeError> =
    let rec unify m t1 t2 =
        match t1, t2 with
        | TCUncertain n, _ ->
            match resolve m t1, resolve m t2 with
            | Ok(t1), Ok(t2) ->
                match t1, t2 with
                | TCUncertain n, _ -> Ok(t2, bindUncertainVar n t2 s)
                | _, TCUncertain n -> Ok(t1, bindUncertainVar n t1 s)
                | _, _ ->
                    if t1 = t2 then
                        Ok(t1, m)
                    else
                        Error(TypeMismatch(Set [ t1; t2 ]))
            | Error(terr), _ -> Error terr // NOTE: Occurence check failed.
            | _, Error(terr) -> Error terr // NOTE: Occurence check failed.
        | _, TCUncertain n ->
            match resolve m t1, resolve m t2 with
            | Ok(t1), Ok(t2) ->
                match t1, t2 with
                | TCUncertain n, _ -> Ok(t2, bindUncertainVar n t2 s)
                | _, TCUncertain n -> Ok(t1, bindUncertainVar n t1 s)
                | _, _ ->
                    if t1 = t2 then
                        Ok(t1, s)
                    else
                        Error(TypeMismatch(Set [ t1; t2 ]))
            | Error(terr), _ -> Error terr // NOTE: Occurence check failed.
            | _, Error(terr) -> Error terr // NOTE: Occurence check failed.
        | TCBool, TCBool -> Ok(TCBool, s)
        | TCNat, TCNat -> Ok(TCNat, s)
        | TCTuple(ts1), TCTuple(ts2) ->
            if List.length ts1 = List.length ts2 then
                Result.map
                    (fun (ts, m) -> (TCTuple(ts), m))
                    (List.foldBack
                        (fun (i, t1, t2) ->
                            Result.bind (fun (ts, m) ->
                                match unify s t1 t2 with
                                | Error terr ->
                                    Error(
                                        At(
                                            terr,
                                            $"the %d{i}th element of the tuple: {TypeCstr.format t1} vs {TypeCstr.format t2}"
                                        )
                                    )
                                | Ok(t, m) -> Ok(t :: ts, m)))
                        (List.mapi (fun i (t1, t2) -> (i, t1, t2)) (List.zip ts1 ts2))
                        (Ok([], s)))
            else
                Error(TupleLengthMismatch(Set [ts1; ts2]))
        | TCUnion(un1, cm1), TCUnion(un2, cm2) ->
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
                                                Result.bind (fun (ts, m) ->
                                                    match unify m t1 t2 with
                                                    | Ok(t, m) -> Ok(t :: ts, s)
                                                    | Error terr ->
                                                        Error(At(terr, $"the type list of %s{Ctor.format ctor}"))))
                                            (List.zip ts1 ts2)
                                            (Ok([], m)))
                                | _ -> Error(CtorsMismatch(ctors1, ctors2)))
                        (Ok(Map.empty, m))
                        ctors

                match cmRes with
                | Ok(cm, m) -> Ok(TCUnion(un1, cm), s)
                | Error terr -> Error terr
            else
                Error(UnionNameMismatch(un1, un2))
        | TCList tcV1, TCList tcV2 ->
            match unify m tcV1 tcV2 with
            | Error terr -> Error(At(terr, $"the element type of the list: {TypeCstr.format t1} vs {TypeCstr.format t2}"))
            | Ok(tcV, m) -> Ok(TCList(tcV), m)
        | TCSet tcV1, TCSet tcV2 ->
            match unify m tcV1 tcV2 with
            | Error terr -> Error(At(terr, $"the element type of the set: {format t1} vs {format t2}"))
            | Ok(tcV, m) -> Ok(TSet(tcV), m)
        | TMap(tcK1, tcV1), TMap(tcK2, tcV2) ->
            // NOTE: Do not use Result.mapError. It make hard to read.
            match unify m tcK1 tcK2 with
            | Error terr -> Error(At(terr, $"the key type of the set: {format t1} vs {format t2}"))
            | Ok(tcK, m) ->
                match unify s tcV1 tcV2 with
                | Error terr -> Error(At(terr, $"the value type of the set: {format t1} vs {format t2}"))
                | Ok(tcV, m) -> Ok(TMap(tcK, tcV), m)
        | _, _ -> Error(TypeMismatch(Set [ t1; t2 ]))

    unify m t1 t2

let generalize (t: Type) (s: InferState) : TypeCstr * InferState * Map<TVarId, UncertainVarId> =
    let rec generalize t s m =
        match t with
        | TVar n ->
            match Map.tryFind n m with
            | Some n' -> (TCUncertain n', s, m)
            | None ->
                let n', s = newUncertainVarId s
                (TCUncertain n', s, Map.add n n' m)
        | TBool -> (TCBool, s, m)
        | TNat -> (TCNat, s, m)
        | TTuple(ts) ->
            let tcs, s, m =
                List.foldBack (fun t (tcs, s, m) -> let tc, s, m = generalize t s m in (tc :: tcs, s, m)) ts ([], s, m)

            (TCTuple tcs, s, m)
        | TUnion(un, cm) ->
            let cm, s, m =
                Map.fold
                    (fun (cm, s, m) ctor ts ->
                        let tcs, s, m =
                            List.foldBack
                                (fun t (tcs, s, m) -> let tc, s, m = generalize t s m in (tc :: tcs, s, m))
                                ts
                                ([], s, m) in

                        let cm = Map.add ctor tcs cm in

                        (cm, s, m))
                    (Map.empty, s, m)
                    cm in

            (TCUnion(un, cm), s, m)
        | TSet tElem ->
            let tc, s, m = generalize t s m
            (TCSet tc, s, m)
        | TList tElem ->
            let tc, s, m = generalize t s m
            (TCList tc, s, m)
        | TMap(tK, tV) ->
            let tcK, s, m = generalize tK s m
            let tcV, s, m = generalize tV s m
            (TCMap(tcK, tcV), s, m)

    generalize t s Map.empty

let infer
    (cm: CtorMap)
    (s: InferState)
    (tenv: TypeEnv)
    (expr: Expr<unit>)
    : Result<Expr<Type> * Map<TVarId, Type>, TypeError> =
    let rec infer n m tenv (expr: Expr<unit>) =
        match expr with
        | LitTrue(_, line) -> Ok(LitTrue(TBool, line), m, n)
        | LitFalse(_, line) -> Ok(LitFalse(TBool, line), m, n)
        | LitNat(n, _, line) -> Ok(LitNat(n, TNat, line), m, n)
        | LitEmpty(t, _, line) ->
            if ClassEmpty.derivedBy t then
                Ok(LitEmpty(t, t, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassEmpty.name)) line)
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
                                            let t' = get expr in

                                            match unify m t t' with
                                            | Error terr -> Error(atLine terr line)
                                            | Ok(_, m) -> Ok(expr :: exprs, m, n))
                                (List.zip ts exprs)
                                (Ok([], m, n)) in

                        Result.map (fun (exprs, m, n) -> (Union(ctor, exprs, t, line), m, n)) exprsRes
                    else
                        Error(UnionValueLenMismatch(ctor, List.length exprs, List.length ts))

        | If(exprCond, exprThen, exprElse, _, line) ->
            match infer n m tenv exprCond with
            | Error terr -> Error(atLine terr line)
            | Ok(exprCond, m, n) ->
                let tcCond = get exprCond in

                match unify m tcCond TBool with
                | Error terr -> Error(atLine terr line)
                | Ok(_, m) ->
                    match infer n m tenv exprThen with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprThen, m, n) ->
                        let tcThen = get exprThen in

                        match infer n m tenv exprElse with
                        | Error terr -> Error(atLine terr line)
                        | Ok(exprElse, m, n) ->
                            let tcElse = get exprElse in

                            match unify m tcThen tcElse with
                            | Error terr -> Error(atLine terr line)
                            | Ok(t, m) -> Ok(If(exprCond, exprThen, exprElse, t, line), m, n)
        | Match(exprUnion, exprMap, _, line) ->
            match infer n m tenv exprUnion with
            | Error terr -> Error(atLine terr line)
            | Ok(exprUnion, m, n) ->
                let tUnion = get exprUnion in

                match tUnion with
                | TUnion(_, cm) ->
                    let inferClause
                        n
                        m
                        ctorOpt
                        varOpts
                        expr
                        : Result<Expr<Type> * Map<TVarId, Type> * TVarId, TypeError> =
                        match ctorOpt with
                        | Some ctor ->
                            match Map.tryFind ctor cm with
                            | None -> Error(atLine (NoSuchCtor ctor) line)
                            | Some ts ->
                                if List.length ts = List.length varOpts then
                                    let tenv =
                                        List.fold
                                            (fun tenv (t, varOpt) ->
                                                match varOpt with
                                                | Some var -> Map.add var t tenv
                                                | None -> tenv)
                                            tenv
                                            (List.zip ts varOpts) in

                                    match infer n m tenv expr with
                                    | Error terr -> Error(atLine terr line)
                                    | Ok(expr, m, n) -> Ok(expr, m, n)
                                else
                                    Error(UnionValueLenMismatch(ctor, List.length varOpts, List.length ts))
                        | None ->
                            Result.bind
                                (fun tenv ->
                                    match infer n m tenv expr with
                                    | Error terr -> Error(atLine terr line)
                                    | Ok(expr, m, n) -> Ok(expr, m, n))
                                (match List.length varOpts with
                                 | 1 ->
                                     match List.head varOpts with
                                     | Some var -> Ok(Map.add var tUnion tenv)
                                     | None -> Ok(tenv)
                                 | _ -> Error(DefaultClauseArgumentsLenMustBe1(varOpts)))


                    let accRes =
                        MapEx.tryFold1
                            (fun accRes ctorOpt (vars, expr) ->
                                match accRes with
                                | Error s -> Error s
                                | Ok(t, exprMap, n, m) ->
                                    Result.bind
                                        (fun (expr, m, n) ->
                                            match unify m t (get expr) with
                                            | Error terr -> Error(atLine terr line)
                                            | Ok(t, m) -> Ok(t, Map.add ctorOpt (vars, expr) exprMap, n, m))
                                        (inferClause n m ctorOpt vars expr))
                            (fun ctorOpt (vars, expr) ->
                                Result.map
                                    (fun (expr, m, n) -> (get expr, Map [ (ctorOpt, (vars, expr)) ], n, m))
                                    (inferClause n m ctorOpt vars expr))
                            exprMap in

                    match accRes with
                    | None -> Error(atLine EmptyMatch line)
                    | Some(Error terr) -> Error(atLine terr line)
                    | Some(Ok(t, exprMap, n, m)) -> Ok(Match(exprUnion, exprMap, t, line), m, n)
                | _ -> Error(atLine (NotUnion(tUnion)) line)
        | VarRef(var, _, line) ->
            match Map.tryFind var tenv with
            | Some t -> Ok(VarRef(var, t, line), m, n)
            | None -> Error(atLine (UnboundVariable var) line)
        | Eq(t, expr1, expr2, _, line) ->
            if ClassEq.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = get expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = get expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcEq, m) ->
                            match unify m tcEq t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, m) -> Ok(Eq(t, expr1, expr2, TBool, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassEq.name)) line)
        | BoolNot(expr, _, line) ->
            match infer n m tenv expr with
            | Error terr -> Error(atLine terr line)
            | Ok(expr, m, n) ->
                let t = get expr in

                match unify m t TBool with
                | Error terr -> Error(atLine terr line)
                | Ok(t, m) -> Ok(BoolNot(expr, t, line), m, n)
        | Less(t, expr1, expr2, _, line) ->
            if ClassOrd.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = get expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = get expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcLess, m) ->
                            match unify m tcLess t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, m) -> Ok(Less(t, expr1, expr2, TBool, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassOrd.name)) line)
        | Plus(t, expr1, expr2, _, line) ->
            if ClassPlus.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = get expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = get expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcPlus, m) ->
                            match unify m tcPlus t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcPlus, m) -> Ok(Plus(t, expr1, expr2, tcPlus, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassPlus.name)) line)
        | Minus(t, expr1, expr2, _, line) ->
            if ClassMinus.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = get expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = get expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcMinus, m) ->
                            match unify m tcMinus t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcMinus, m) -> Ok(Minus(t, expr1, expr2, tcMinus, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassMinus.name)) line)
        | Times(t, expr1, expr2, _, line) ->
            if ClassTimes.derivedBy t then
                match infer n m tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, m, n) ->
                    let t1 = get expr1 in

                    match infer n m tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, m, n) ->
                        let t2 = get expr2 in

                        match unify m t1 t2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcTimes, m) ->
                            match unify m tcTimes t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcTimes, m) -> Ok(Times(t, expr1, expr2, tcTimes, line), m, n)
            else
                Error(atLine (TypeNotDerived(t, ClassTimes.name)) line)
        | Size(t, expr, _, line) ->
            if ClassSize.derivedBy t then
                match infer n m tenv expr with
                | Error terr -> Error(atLine terr line)
                | Ok(expr, m, n) ->
                    let t' = get expr in

                    match unify m t t' with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, m) -> Ok(Size(t, expr, TNat, line), m, n)
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
                    let t1 = get expr1 in

                    match unify m t1 TBool with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, m) ->
                        match infer n m tenv expr2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(expr2, m, n) ->
                            let t2 = get expr2 in

                            match unify m t2 t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcFilter, m) -> Ok(Filter(t, var, expr1, expr2, tcFilter, line), m, n)
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
                    let t1 = get expr1 in

                    match unify m t1 TBool with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, m) ->
                        match infer n m tenv expr2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(expr2, m, n) ->
                            let t2 = get expr2 in

                            match unify m t2 t with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, m) -> Ok(Exists(t, var, expr1, expr2, TBool, line), m, n)
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
            | Ok(exprs, m, n) -> Ok(Tuple(exprs, TTuple(List.map get exprs), line), m, n)
            | Error terr -> Error(atLine terr line)
        | TupleNth(exprTuple, idx, _, line) ->
            match infer n m tenv exprTuple with
            | Error terr -> Error(atLine terr line)
            | Ok(exprTuple, m, n) ->
                let tcTuple = get exprTuple in

                match tcTuple with
                | TTuple(ts) ->
                    match List.tryItem (Checked.int idx) ts with
                    | Some t -> Ok(TupleNth(exprTuple, idx, t, line), m, n)
                    | None -> Error(At(TupleIndexOutOfBounds(tcTuple, idx), $"line %s{line}"))
                | _ -> Error(At(NotTuple(tcTuple), $"line %s{line}"))

        | ListCons(exprElem, exprList, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, m, n) ->
                let tcElem = get exprElem in

                match infer n m tenv exprList with
                | Error terr -> Error(atLine terr line)
                | Ok(exprList, m, n) ->
                    let tcList = get exprList in

                    match unify m tcList (TList tcElem) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(tcList, m) -> Ok(ListCons(exprElem, exprList, tcList, line), m, n)
        | ListNth(exprList, exprIdx, _, line) ->
            match infer n m tenv exprList with
            | Error terr -> Error(atLine terr line)
            | Ok(exprList, m, n) ->
                let tcList = get exprList in

                match unify m tcList (TList(TVar n)) with
                | Error terr -> Error(atLine terr line)
                | Ok(TList(tcElem), m) ->
                    match infer (n + 1u) m tenv exprIdx with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprIdx, m, n) ->
                        let tcIdx = get exprIdx in

                        match unify m tcIdx TNat with
                        | Error terr -> Error(atLine terr line)
                        | Ok(_, m) -> Ok(ListNth(exprList, exprIdx, tcElem, line), m, n)
                | x -> failwith $"unification between TList and any must return TList, but come: %A{x}"
        | SetRange(exprLower, exprUpper, _, line) ->
            match infer n m tenv exprLower with
            | Error terr -> Error(atLine terr line)
            | Ok(exprLower, m, n) ->
                let tcLower = get exprLower in

                match unify m tcLower TNat with
                | Error terr -> Error(atLine terr line)
                | Ok(_, m) ->
                    match infer n m tenv exprUpper with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprUpper, m, n) ->
                        let tcUpper = get exprUpper in

                        match unify m tcUpper TNat with
                        | Error terr -> Error(atLine terr line)
                        | Ok(_, m) -> Ok(ListNth(exprLower, exprUpper, TSet(TNat), line), m, n)

        | SetInsert(exprElem, exprSet, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, m, n) ->
                let tcElem = get exprElem in

                match infer n m tenv exprSet with
                | Error terr -> Error(atLine terr line)
                | Ok(exprSet, m, n) ->
                    let tcSet = get exprSet in

                    match unify m tcSet (TSet(tcElem)) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(tcSet, m) -> Ok(SetInsert(exprElem, exprSet, tcSet, line), m, n)

        | SetMem(exprElem, exprSet, _, line) ->
            match infer n m tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, m, n) ->
                let tcElem = get exprElem in

                match infer n m tenv exprSet with
                | Error terr -> Error(atLine terr line)
                | Ok(exprSet, m, n) ->
                    let tcSet = get exprSet in

                    match unify m tcSet (TSet(tcElem)) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, m) -> Ok(SetMem(exprElem, exprSet, TBool, line), m, n)

        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            match infer n m tenv exprKey with
            | Error terr -> Error(atLine terr line)
            | Ok(exprKey, m, n) ->
                let tcKey = get exprKey in

                match infer n m tenv exprVal with
                | Error terr -> Error(atLine terr line)
                | Ok(exprVal, m, n) ->
                    let tcVal = get exprVal in

                    match infer n m tenv exprMap with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprMap, m, n) ->
                        let tcMap = get exprMap in

                        match unify m tcMap (TMap(tcKey, tcVal)) with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcMap, m) -> Ok(MapAdd(exprKey, exprVal, exprMap, tcMap, line), m, n)

        | MapFindOpt(exprKey, exprMap, _, line) ->
            match infer n m tenv exprKey with
            | Error terr -> Error(atLine terr line)
            | Ok(exprKey, m, n) ->
                let tcKey = get exprKey in

                match infer n m tenv exprMap with
                | Error terr -> Error(atLine terr line)
                | Ok(exprMap, m, n) ->
                    let tcMap = get exprMap in

                    match unify m tcMap (TMap(tcKey, TVar(n))) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(TMap(_, tVal), m) ->
                        Ok(
                            MapFindOpt(
                                exprKey,
                                exprMap,
                                TUnion("option", Map [ (Ctor "Some", [ tVal ]); Ctor "None", [] ]),
                                line
                            ),
                            m,
                            n + 1u
                        )
                    | x -> failwith $"unification between TMap and any must return TMap, but come: %A{x}"
        | Univ(t, _, line) -> Ok(Univ(t, TSet t, line), m, n)

    match infer n tenv expr with
    | Error err -> Error err
    | Ok(expr, m, _) ->
        let expr = map (get >> resolve m) expr in

        let exprRes =
            fold (fun accRes expr -> Result.bind (fun _ -> Result.map (fun _ -> ()) (get expr)) accRes) (Ok(())) expr

        match exprRes with
        | Ok _ ->
            Ok(
                map
                    (fun expr ->
                        match get expr with
                        | Ok t -> t
                        | Error err -> failwith (formatTypeError err))
                    expr
            )
        | Error err -> Error err
