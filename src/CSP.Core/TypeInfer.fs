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
    | NotUnion of TypeCstr
    | NotTuple of TypeCstr
    | TupleIndexOutOfBounds of TypeCstr * uint
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
        | NotUnion t -> $"not union: %s{TypeCstr.format t}"
        | NotTuple t -> $"not tuple: %s{TypeCstr.format t}"
        | TupleIndexOutOfBounds(t, idx) -> $"tuple index out of bounds: %s{TypeCstr.format t} at %d{idx}"
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

let initState: InferState = { UncertainVarMap = TypeCstrUncertainVar.init }

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

let unify (s: InferState) (tc1: TypeCstr) (tc2: TypeCstr) : Result<TypeCstr * InferState, TypeError> =
    let rec unify s tc1 tc2 =
        match resolve s tc1, resolve s tc2 with
        | Error(terr), _ -> Error terr // NOTE: Occurence check failed.
        | _, Error(terr) -> Error terr // NOTE: Occurence check failed.
        | Ok(tc1), Ok(tc2) ->
            match tc1, tc2 with
            | TCUncertain n, _ -> Ok(tc2, bindUncertainVar n tc2 s)
            | _, TCUncertain n -> Ok(tc1, bindUncertainVar n tc1 s)
            | TCBool, TCBool -> Ok(TCBool, s)
            | TCNat, TCNat -> Ok(TCNat, s)
            | TCTuple(tcs1), TCTuple(tcs2) ->
                if List.length tcs1 = List.length tcs2 then
                    Result.map
                        (fun (tcs, m) -> (TCTuple(tcs), m))
                        (List.foldBack
                            (fun (i, tc1, tc2) ->
                                Result.bind (fun (tcs, s) ->
                                    match unify s tc1 tc2 with
                                    | Error terr ->
                                        Error(
                                            At(
                                                terr,
                                                $"the %d{i}th element of the tuple: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"
                                            )
                                        )
                                    | Ok(tc, m) -> Ok(tc :: tcs, m)))
                            (List.mapi (fun i (tc1, tc2) -> (i, tc1, tc2)) (List.zip tcs1 tcs2))
                            (Ok([], s)))
                else
                    Error(TupleLengthMismatch(Set [ tcs1; tcs2 ]))
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
                                    | Some tcs1, Some tcs2 ->
                                        Result.map
                                            (fun (tcs, m) -> (Map.add ctor tcs cm, m))
                                            (List.foldBack
                                                (fun (tc1, tc2) ->
                                                    Result.bind (fun (tcs, m) ->
                                                        match unify m tc1 tc2 with
                                                        | Ok(tc, s) -> Ok(tc :: tcs, s)
                                                        | Error terr ->
                                                            Error(At(terr, $"the type list of %s{Ctor.format ctor}"))))
                                                (List.zip tcs1 tcs2)
                                                (Ok([], m)))
                                    | _ -> Error(CtorsMismatch(ctors1, ctors2)))
                            (Ok(Map.empty, s))
                            ctors

                    match cmRes with
                    | Ok(cm, s) -> Ok(TCUnion(un1, cm), s)
                    | Error terr -> Error terr
                else
                    Error(UnionNameMismatch(un1, un2))
            | TCList tcV1, TCList tcV2 ->
                match unify s tcV1 tcV2 with
                | Error terr ->
                    Error(At(terr, $"the element type of the list: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"))
                | Ok(tcV, s) -> Ok(TCList(tcV), s)
            | TCSet tcV1, TCSet tcV2 ->
                match unify s tcV1 tcV2 with
                | Error terr ->
                    Error(At(terr, $"the element type of the set: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"))
                | Ok(tcV, s) -> Ok(TCSet(tcV), s)
            | TCMap(tcK1, tcV1), TCMap(tcK2, tcV2) ->
                // NOTE: Do not use Result.mapError. It make hard to read.
                match unify s tcK1 tcK2 with
                | Error terr ->
                    Error(At(terr, $"the key type of the set: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"))
                | Ok(tcK, s) ->
                    match unify s tcV1 tcV2 with
                    | Error terr ->
                        Error(At(terr, $"the value type of the set: {TypeCstr.format tc1} vs {TypeCstr.format tc2}"))
                    | Ok(tcV, s) -> Ok(TCMap(tcK, tcV), s)
            | _, _ -> Error(TypeMismatch(Set [ tc1; tc2 ]))

    unify s tc1 tc2

let generalize (t: Type) (s: InferState) : TypeCstr * InferState =
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
            let tc, s, m = generalize tElem s m
            (TCSet tc, s, m)
        | TList tElem ->
            let tc, s, m = generalize tElem s m
            (TCList tc, s, m)
        | TMap(tK, tV) ->
            let tcK, s, m = generalize tK s m
            let tcV, s, m = generalize tV s m
            (TCMap(tcK, tcV), s, m)

    let tc, s, _ = generalize t s Map.empty
    (tc, s)

let generalizeAll (ts: Type list) (s: InferState) : TypeCstr list * InferState =
    List.foldBack (fun t (tcs, s) -> let tc, s = generalize t s in (tc :: tcs, s)) ts ([], s)

let rec instantiate (tc: TypeCstr) : Type =
    match tc with
    | TCUncertain(UncertainVarId u) -> TVar u
    | TCBool -> TBool
    | TCNat -> TNat
    | TCTuple(tcs) -> TTuple(List.map instantiate tcs)
    | TCUnion(un, cm) -> TUnion(un, Map.map (fun _ -> List.map instantiate) cm)
    | TCSet(tc) -> TSet(instantiate tc)
    | TCList(tc) -> TList(instantiate tc)
    | TCMap(tcK, tcV) -> TMap(instantiate tcK, instantiate tcV)

let infer (cm: CtorMap) (tenv: TypeEnv) (expr: Expr<unit>) : Result<Expr<Type> * InferState, TypeError> =
    let rec infer s tenv expr =
        match expr with
        | LitTrue(_, line) -> Ok(LitTrue(TCBool, line), s)
        | LitFalse(_, line) -> Ok(LitFalse(TCBool, line), s)
        | LitNat(n, _, line) -> Ok(LitNat(n, TCNat, line), s)
        | LitEmpty(t, _, line) ->
            if ClassEmpty.derivedBy t then
                let tc, s = generalize t s in Ok(LitEmpty(t, tc, line), s)
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
                        let cm, s =
                            Map.fold
                                (fun (cm, s) ctor ts -> let tcs, s = generalizeAll ts s in (Map.add ctor tcs cm, s))
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
                                        match infer s tenv expr with
                                        | Error terr -> Error(atLine terr line)
                                        | Ok(expr, s) ->
                                            let tc' = get expr in

                                            match unify s tc tc' with
                                            | Error terr -> Error(atLine terr line)
                                            | Ok(_, s) -> Ok(expr :: exprs, s))
                                (List.zip tcs exprs)
                                (Ok([], s)) in

                        Result.map (fun (exprs, s) -> (Union(ctor, exprs, t, line), s)) exprsRes
                    else
                        Error(UnionValueLenMismatch(ctor, List.length exprs, List.length ts))

        | If(exprCond, exprThen, exprElse, _, line) ->
            match infer s tenv exprCond with
            | Error terr -> Error(atLine terr line)
            | Ok(exprCond, s) ->
                let tcCond = get exprCond in

                match unify s tcCond TCBool with
                | Error terr -> Error(atLine terr line)
                | Ok(_, s) ->
                    match infer s tenv exprThen with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprThen, s) ->
                        let tcThen = get exprThen in

                        match infer s tenv exprElse with
                        | Error terr -> Error(atLine terr line)
                        | Ok(exprElse, s) ->
                            let tcElse = get exprElse in

                            match unify s tcThen tcElse with
                            | Error terr -> Error(atLine terr line)
                            | Ok(t, s) -> Ok(If(exprCond, exprThen, exprElse, t, line), s)
        | Match(exprUnion, exprMap, _, line) ->
            match infer s tenv exprUnion with
            | Error terr -> Error(atLine terr line)
            | Ok(exprUnion, s) ->
                let tUnion = get exprUnion in

                match tUnion with
                | TCUnion(_, cm) ->
                    let inferClause s ctorOpt varOpts expr : Result<Expr<TypeCstr> * InferState, TypeError> =
                        match ctorOpt with
                        | Some ctor ->
                            match Map.tryFind ctor cm with
                            | None -> Error(atLine (NoSuchCtor ctor) line)
                            | Some tcs ->
                                if List.length tcs = List.length varOpts then
                                    let tenv =
                                        List.fold
                                            (fun tenv (tc, varOpt) ->
                                                match varOpt with
                                                | Some var -> Map.add var tc tenv
                                                | None -> tenv)
                                            tenv
                                            (List.zip tcs varOpts) in

                                    match infer s tenv expr with
                                    | Error terr -> Error(atLine terr line)
                                    | Ok(expr, s) -> Ok(expr, s)
                                else
                                    Error(UnionValueLenMismatch(ctor, List.length varOpts, List.length tcs))
                        | None ->
                            Result.bind
                                (fun tenv ->
                                    match infer s tenv expr with
                                    | Error terr -> Error(atLine terr line)
                                    | Ok(expr, s) -> Ok(expr, s))
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
                                | Ok(tc, exprMap, s) ->
                                    Result.bind
                                        (fun (expr, s) ->
                                            match unify s tc (get expr) with
                                            | Error terr -> Error(atLine terr line)
                                            | Ok(tc, s) -> Ok(tc, Map.add ctorOpt (vars, expr) exprMap, s))
                                        (inferClause s ctorOpt vars expr))
                            (fun ctorOpt (vars, expr) ->
                                Result.map
                                    (fun (expr, s) -> (get expr, Map [ (ctorOpt, (vars, expr)) ], s))
                                    (inferClause s ctorOpt vars expr))
                            exprMap in

                    match accRes with
                    | None -> Error(atLine EmptyMatch line)
                    | Some(Error terr) -> Error(atLine terr line)
                    | Some(Ok(tc, exprMap, s)) -> Ok(Match(exprUnion, exprMap, tc, line), s)
                | _ -> Error(atLine (NotUnion(tUnion)) line)
        | VarRef(var, _, line) ->
            match Map.tryFind var tenv with
            | Some tc -> Ok(VarRef(var, tc, line), s)
            | None -> Error(atLine (UnboundVariable var) line)
        | Eq(t, expr1, expr2, _, line) ->
            if ClassEq.derivedBy t then
                match infer s tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcEq, s) ->
                            let tc, s = generalize t s in

                            match unify s tcEq tc with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, s) -> Ok(Eq(t, expr1, expr2, TCBool, line), s)
            else
                Error(atLine (TypeNotDerived(t, ClassEq.name)) line)
        | BoolNot(expr, _, line) ->
            match infer s tenv expr with
            | Error terr -> Error(atLine terr line)
            | Ok(expr, s) ->
                let t = get expr in

                match unify s t TCBool with
                | Error terr -> Error(atLine terr line)
                | Ok(t, s) -> Ok(BoolNot(expr, t, line), s)
        | Less(t, expr1, expr2, _, line) ->
            if ClassOrd.derivedBy t then
                match infer s tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcLess, s) ->
                            let tc, s = generalize t s in

                            match unify s tcLess tc with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, s) -> Ok(Less(t, expr1, expr2, TCBool, line), s)
            else
                Error(atLine (TypeNotDerived(t, ClassOrd.name)) line)
        | Plus(t, expr1, expr2, _, line) ->
            if ClassPlus.derivedBy t then
                match infer s tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcPlus, s) ->
                            let tc, s = generalize t s in

                            match unify s tcPlus tc with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcPlus, s) -> Ok(Plus(t, expr1, expr2, tcPlus, line), s)
            else
                Error(atLine (TypeNotDerived(t, ClassPlus.name)) line)
        | Minus(t, expr1, expr2, _, line) ->
            if ClassMinus.derivedBy t then
                match infer s tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcMinus, s) ->
                            let tc, s = generalize t s in

                            match unify s tcMinus tc with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcMinus, s) -> Ok(Minus(t, expr1, expr2, tcMinus, line), s)
            else
                Error(atLine (TypeNotDerived(t, ClassMinus.name)) line)
        | Times(t, expr1, expr2, _, line) ->
            if ClassTimes.derivedBy t then
                match infer s tenv expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match infer s tenv expr2 with
                    | Error terr -> Error(atLine terr line)
                    | Ok(expr2, s) ->
                        let tc2 = get expr2 in

                        match unify s tc1 tc2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcTimes, s) ->
                            let tc, s = generalize t s in

                            match unify s tcTimes tc with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcTimes, s) -> Ok(Times(t, expr1, expr2, tcTimes, line), s)
            else
                Error(atLine (TypeNotDerived(t, ClassTimes.name)) line)
        | Size(t, expr, _, line) ->
            if ClassSize.derivedBy t then
                match infer s tenv expr with
                | Error terr -> Error(atLine terr line)
                | Ok(expr, s) ->
                    let tc, s = generalize t s in
                    let tc' = get expr in

                    match unify s tc tc' with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, s) -> Ok(Size(t, expr, TCNat, line), s)
            else
                Error(atLine (TypeNotDerived(t, ClassSize.name)) line)
        | Filter(t, var, expr1, expr2, _, line) ->
            if ClassEnum.derivedBy t then
                let elemTC, s =
                    match t with
                    | TSet tElem -> generalize tElem s
                    | TList tElem -> generalize tElem s
                    | TMap(tK, _) -> generalize tK s
                    | _ -> failwith $"cannot get element type: %s{format t}" in

                match infer s (Map.add var elemTC tenv) expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match unify s tc1 TCBool with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, s) ->
                        match infer s tenv expr2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(expr2, s) ->
                            let tc, s = generalize t s in
                            let tc2 = get expr2 in

                            match unify s tc2 tc with
                            | Error terr -> Error(atLine terr line)
                            | Ok(tcFilter, s) -> Ok(Filter(t, var, expr1, expr2, tcFilter, line), s)
            else
                Error(atLine (TypeNotDerived(t, ClassEnum.name)) line)
        | Exists(t, var, expr1, expr2, _, line) ->
            if ClassEnum.derivedBy t then
                let elemTC, s =
                    match t with
                    | TSet tElem -> generalize tElem s
                    | TList tElem -> generalize tElem s
                    | TMap(tK, _) -> generalize tK s
                    | _ -> failwith $"cannot get element type: %s{format t}" in

                match infer s (Map.add var elemTC tenv) expr1 with
                | Error terr -> Error(atLine terr line)
                | Ok(expr1, s) ->
                    let tc1 = get expr1 in

                    match unify s tc1 TCBool with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, s) ->
                        match infer s tenv expr2 with
                        | Error terr -> Error(atLine terr line)
                        | Ok(expr2, s) ->
                            let tc2 = get expr2 in
                            let tc, s = generalize t s in

                            match unify s tc2 tc with
                            | Error terr -> Error(atLine terr line)
                            | Ok(_, s) -> Ok(Exists(t, var, expr1, expr2, TCBool, line), s)
            else
                Error(atLine (TypeNotDerived(t, ClassEnum.name)) line)
        | Tuple(exprs, _, line) ->
            let accRes =
                List.foldBack
                    (fun expr accRes ->
                        match accRes with
                        | Error terr -> Error terr
                        | Ok(exprs, s) ->
                            match infer s tenv expr with
                            | Error terr -> Error terr
                            | Ok(expr, s) -> Ok(expr :: exprs, s))
                    exprs
                    (Ok([], s)) in

            match accRes with
            | Ok(exprs, s) -> Ok(Tuple(exprs, TCTuple(List.map get exprs), line), s)
            | Error terr -> Error(atLine terr line)
        | TupleNth(exprTuple, idx, _, line) ->
            match infer s tenv exprTuple with
            | Error terr -> Error(atLine terr line)
            | Ok(exprTuple, s) ->
                let tcTuple = get exprTuple in

                match tcTuple with
                | TCTuple(ts) ->
                    match List.tryItem (Checked.int idx) ts with
                    | Some t -> Ok(TupleNth(exprTuple, idx, t, line), s)
                    | None -> Error(At(TupleIndexOutOfBounds(tcTuple, idx), $"line %s{line}"))
                | _ -> Error(At(NotTuple(tcTuple), $"line %s{line}"))

        | ListCons(exprElem, exprList, _, line) ->
            match infer s tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, s) ->
                let tcElem = get exprElem in

                match infer s tenv exprList with
                | Error terr -> Error(atLine terr line)
                | Ok(exprList, s) ->
                    let tcList = get exprList in

                    match unify s tcList (TCList tcElem) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(tcList, s) -> Ok(ListCons(exprElem, exprList, tcList, line), s)
        | ListNth(exprList, exprIdx, _, line) ->
            match infer s tenv exprList with
            | Error terr -> Error(atLine terr line)
            | Ok(exprList, s) ->
                let tcList = get exprList in
                let u, s = newUncertainVarId s in

                match unify s tcList (TCList(TCUncertain u)) with
                | Error terr -> Error(atLine terr line)
                | Ok(TCList(tcElem), s) ->
                    match infer s tenv exprIdx with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprIdx, s) ->
                        let tcIdx = get exprIdx in

                        match unify s tcIdx TCNat with
                        | Error terr -> Error(atLine terr line)
                        | Ok(_, s) -> Ok(ListNth(exprList, exprIdx, tcElem, line), s)
                | x -> failwith $"unification between TList and any must return TList, but come: %A{x}"
        | SetRange(exprLower, exprUpper, _, line) ->
            match infer s tenv exprLower with
            | Error terr -> Error(atLine terr line)
            | Ok(exprLower, s) ->
                let tcLower = get exprLower in

                match unify s tcLower TCNat with
                | Error terr -> Error(atLine terr line)
                | Ok(_, s) ->
                    match infer s tenv exprUpper with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprUpper, s) ->
                        let tcUpper = get exprUpper in

                        match unify s tcUpper TCNat with
                        | Error terr -> Error(atLine terr line)
                        | Ok(_, s) -> Ok(ListNth(exprLower, exprUpper, TCSet(TCNat), line), s)

        | SetInsert(exprElem, exprSet, _, line) ->
            match infer s tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, s) ->
                let tcElem = get exprElem in

                match infer s tenv exprSet with
                | Error terr -> Error(atLine terr line)
                | Ok(exprSet, s) ->
                    let tcSet = get exprSet in

                    match unify s tcSet (TCSet(tcElem)) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(tcSet, s) -> Ok(SetInsert(exprElem, exprSet, tcSet, line), s)

        | SetMem(exprElem, exprSet, _, line) ->
            match infer s tenv exprElem with
            | Error terr -> Error(atLine terr line)
            | Ok(exprElem, s) ->
                let tcElem = get exprElem in

                match infer s tenv exprSet with
                | Error terr -> Error(atLine terr line)
                | Ok(exprSet, s) ->
                    let tcSet = get exprSet in

                    match unify s tcSet (TCSet(tcElem)) with
                    | Error terr -> Error(atLine terr line)
                    | Ok(_, s) -> Ok(SetMem(exprElem, exprSet, TCBool, line), s)

        | MapAdd(exprKey, exprVal, exprMap, _, line) ->
            match infer s tenv exprKey with
            | Error terr -> Error(atLine terr line)
            | Ok(exprKey, s) ->
                let tcKey = get exprKey in

                match infer s tenv exprVal with
                | Error terr -> Error(atLine terr line)
                | Ok(exprVal, s) ->
                    let tcVal = get exprVal in

                    match infer s tenv exprMap with
                    | Error terr -> Error(atLine terr line)
                    | Ok(exprMap, s) ->
                        let tcMap = get exprMap in

                        match unify s tcMap (TCMap(tcKey, tcVal)) with
                        | Error terr -> Error(atLine terr line)
                        | Ok(tcMap, s) -> Ok(MapAdd(exprKey, exprVal, exprMap, tcMap, line), s)

        | MapFindOpt(exprKey, exprMap, _, line) ->
            match infer s tenv exprKey with
            | Error terr -> Error(atLine terr line)
            | Ok(exprKey, s) ->
                let tcKey = get exprKey in

                match infer s tenv exprMap with
                | Error terr -> Error(atLine terr line)
                | Ok(exprMap, s) ->
                    let tcMap = get exprMap in
                    let u, s = newUncertainVarId s in

                    match unify s tcMap (TCMap(tcKey, TCUncertain u)) with
                    | Error terr -> Error(atLine terr line)
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

    let tcenv, s =
        Map.fold
            (fun (tcenv, s) var t -> let tc, s = generalize t s in (Map.add var tc tcenv, s))
            (Map.empty, initState)
            tenv

    match infer s tcenv expr with
    | Error err -> Error err
    | Ok(expr, s) ->
        let expr = map (get >> resolve s) expr in

        let exprRes =
            fold (fun accRes expr -> Result.bind (fun _ -> Result.map (fun _ -> ()) (get expr)) accRes) (Ok(())) expr

        match exprRes with
        | Ok _ ->
            Ok(
                map
                    (fun expr ->
                        match get expr with
                        | Ok tc -> instantiate tc
                        | Error err -> failwith (formatTypeError err))
                    expr,
                s
            )
        | Error err -> Error err
