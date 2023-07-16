module CSP.Core.TypeCstr

open CSP.Core.Ctor
open CSP.Core.Type
open CSP.Core.Val
open CSP.Core.Expr

type TypeCstr =
    | TCVar of uint
    | TCName of string
    | TCUnit
    | TCNat
    | TCBool
    | TCTuple of TypeCstr * TypeCstr
    | TCSet of TypeCstr
    | TCList of TypeCstr
    | TCMap of TypeCstr * TypeCstr
    | TCUnion of TypeCstr * TypeCstr
    | TCError

type TypeEnv<'Var when 'Var: comparison> = Map<'Var, TypeCstr>

let rec format (tc: TypeCstr) =
    match tc with
    | TCVar n -> $"'t{n}"
    | TCName n -> n
    | TCUnit -> "unit"
    | TCNat -> "nat"
    | TCBool -> "bool"
    | TCTuple(tcL, tcR) -> $"({format tcL} * {format tcR})"
    | TCSet(tc) -> $"({format tc} set)"
    | TCList(tc) -> $"({format tc} list)"
    | TCMap(tcK, tcV) -> $"(({format tcK}, {format tcV}) map)"
    | TCUnion(name, t) -> $"({format t} {format name})"
    | TCError -> "ERROR"

let merge (m1: Map<'K, 'V>) (m2: Map<'K, 'V>) : Map<'K, 'V> =
    Map.fold (fun acc k v -> Map.add k v acc) m1 m2

let rec ofType (t: Type) : TypeCstr =
    match t with
    | TUnit -> TCUnit
    | TNat -> TCNat
    | TBool -> TCBool
    | TTuple(tL, tR) -> TCTuple(ofType tL, ofType tR)
    | TSet(t) -> TCSet(ofType t)
    | TList(t) -> TCList(ofType t)
    | TMap(tK, tV) -> TCMap(ofType tK, ofType tV)
    | TUnion(name, t) -> TCUnion(TCName name, ofType t)
    | TError -> TCError

type TCVarNumberNext = uint
type TCVarNumberRecent = uint
type TCVarNumber = uint

let newVar (p: uint * (uint * TypeCstr) list) : TCVarNumberRecent * (TCVarNumberNext * (TCVarNumber * TypeCstr) list) =
    let n, m = p in (n, (n + 1u, m))

let newCstrs
    (tcs: TypeCstr list)
    (p: uint * (uint * TypeCstr) list)
    : TCVarNumberRecent * (TCVarNumberNext * (TCVarNumber * TypeCstr) list) =
    let n, m = p in (n, (n + 1u, (List.map (fun tc -> (n, tc)) tcs) @ m))

let ofVal
    (cm: Map<Ctor<'Ctor>, string * Type>)
    (acc: uint * (uint * TypeCstr) list)
    (v: Val<'Ctor>)
    : TCVarNumberRecent * (TCVarNumberNext * (TCVarNumber * TypeCstr) list) =
    let rec ofVal acc v =
        match v with
        | VUnit -> newCstrs [ TCUnit ] acc
        | VNat _ -> newCstrs [ TCNat ] acc
        | VBool _ -> newCstrs [ TCBool ] acc
        | VTuple(vL, vR) ->
            let nL, acc = ofVal acc vL in
            let nR, acc = ofVal acc vR in
            newCstrs [ TCTuple(TCVar nL, TCVar nR) ] acc
        | VSet(vs) ->
            let nV, acc = newVar acc

            newCstrs
                [ TCSet(TCVar nV) ]
                (Set.fold
                    (fun acc v ->
                        let nV', acc = ofVal acc v in
                        let _, acc = newCstrs [ TCVar nV'; TCVar nV ] acc in
                        acc)
                    acc
                    vs)
        | VList(vs) ->
            let nV, acc = newVar acc in

            newCstrs
                [ TCList(TCVar nV) ]
                (List.fold
                    (fun acc v ->
                        let nV', acc = ofVal acc v in
                        let _, acc = newCstrs [ TCVar nV'; TCVar nV ] acc in
                        acc)
                    acc
                    vs)
        | VMap(vs) ->
            let nK, acc = newVar acc in
            let nV, acc = newVar acc in

            newCstrs
                [ TCMap(TCVar nK, TCVar nV) ]
                (Map.fold
                    (fun acc k v ->
                        let nK', acc = ofVal acc k in
                        let _, acc = newCstrs [ TCVar nK'; TCVar nK ] acc in
                        let nV', acc = ofVal acc v in
                        let _, acc = newCstrs [ TCVar nV'; TCVar nV ] acc in
                        acc)
                    acc
                    vs)
        | VUnion(ctor, v) ->
            let name, t = Map.find ctor cm in
            let nN, acc = newCstrs [ TCName name ] acc in
            let nV, acc = ofVal acc v in
            let _, acc = newCstrs [ ofType t; TCVar nV ] acc in
            newCstrs [ TCUnion(TCVar nN, TCVar nV) ] acc
        | VError -> newCstrs [ TCError ] acc

    ofVal acc v

let ofExpr
    (cm: Map<Ctor<'Ctor>, string * Type>)
    (tenv: TypeEnv<'Var>)
    (acc: uint * (uint * TypeCstr) list)
    (expr: Expr<'Var, 'Ctor>)
    : TCVarNumberRecent * (TCVarNumberNext * (TCVarNumber * TypeCstr) list) =
    let rec ofExpr tenv acc expr =
        match expr with
        | Lit v -> ofVal cm acc v
        | Union(ctor, expr) ->
            let name, tV = Map.find ctor cm in
            let nN, acc = newCstrs [ TCName name ] acc in
            let nE, acc = ofExpr tenv acc expr in
            let nV, acc = newCstrs [ ofType tV; TCVar nE ] acc in
            newCstrs [ TCUnion(TCVar nN, TCVar nV) ] acc
        | UnionEq(name, expr1, expr2) ->
            let nUV, acc = newVar acc in
            let nU, acc = newCstrs [ TCUnion(TCName name, TCVar nUV) ] acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nU ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCVar nU ] acc in
            newCstrs [ TCBool ] acc
        | Throw -> newCstrs [ TCError ] acc
        | ErrorEq(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCError ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCError ] acc in
            newCstrs [ TCBool ] acc
        | If(expr1, expr2, expr3) ->
            let nV, acc = newVar acc in
            let nCond, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCBool; TCVar nCond ] acc
            let nVThen, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar nV; TCVar nVThen ] acc
            let nVElse, acc = ofExpr tenv acc expr3 in
            let _, acc = newCstrs [ TCVar nV; TCVar nVElse ] acc
            newCstrs [ TCVar nV ] acc
        | Match(expr, me, de) ->
            let nUN, acc = newVar acc in
            let nUV, acc = newVar acc in
            let nE, acc = newCstrs [ TCUnion(TCVar nUN, TCVar nUV) ] acc in
            let nE', acc = ofExpr tenv acc expr in
            let nV, acc = newVar acc in
            let _, acc = newCstrs [ TCVar nE; TCVar nE' ] acc in

            let acc =
                Map.fold
                    (fun acc ctor (var, expr) ->
                        let name, tV = Map.find ctor cm in
                        let _, acc = newCstrs [ TCVar nUN; TCName name ] acc in
                        let nVar, acc = newCstrs [ ofType tV ] acc in
                        let tenv = Map.add var (TCVar nVar) tenv in
                        let nV', acc = ofExpr tenv acc expr in
                        let _, acc = newCstrs [ TCVar nV; TCVar nV' ] acc in
                        acc)
                    acc
                    me in

            let acc =
                match de with
                | Some(Some var, expr) ->
                    let tenv = Map.add var (TCVar nE) tenv in
                    let nV', acc = ofExpr tenv acc expr in
                    snd (newCstrs [ TCVar nV; TCVar nV' ] acc)
                | Some(None, expr) -> let nV', acc = ofExpr tenv acc expr in snd (newCstrs [ TCVar nV; TCVar nV' ] acc)
                | None -> acc in

            newCstrs [ TCVar nV ] acc
        | VarRef var ->
            match Map.tryFind var tenv with
            | Some tc -> newCstrs [ tc ] acc
            | None -> newVar acc
        | BoolEq(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCBool ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCBool ] acc in
            newCstrs [ TCBool ] acc
        | BoolNot expr -> let n, acc = ofExpr tenv acc expr in newCstrs [ TCVar n; TCBool ] acc
        | BoolAnd(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCBool ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCBool ] acc in
            newCstrs [ TCBool ] acc
        | BoolOr(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCBool ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCBool ] acc in
            newCstrs [ TCBool ] acc
        | NatEq(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCNat ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCNat ] acc in
            newCstrs [ TCBool ] acc
        | NatLess(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCNat ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCNat ] acc in
            newCstrs [ TCBool ] acc
        | NatAdd(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCNat ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCNat ] acc in
            newCstrs [ TCNat ] acc
        | NatProd(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCNat ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCNat ] acc in
            newCstrs [ TCNat ] acc
        | NatSub(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCNat ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCNat ] acc in
            newCstrs [ TCNat ] acc
        | Tuple(exprL, exprR) ->
            let nL, acc = ofExpr tenv acc exprL in
            let nR, acc = ofExpr tenv acc exprR in
            newCstrs [ TCTuple(TCVar nL, TCVar nR) ] acc
        | TupleEq(expr1, expr2) ->
            let nL, acc = newVar acc in
            let nR, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nL ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCVar nR ] acc in
            newCstrs [ TCBool ] acc
        | TupleFst(expr) ->
            let nL, acc = newVar acc in
            let nR, acc = newVar acc in
            let nE, acc = ofExpr tenv acc expr in
            let _, acc = newCstrs [ TCVar nE; TCTuple(TCVar nL, TCVar nR) ] acc in
            newCstrs [ TCVar nL ] acc
        | TupleSnd(expr) ->
            let nL, acc = newVar acc in
            let nR, acc = newVar acc in
            let nE, acc = ofExpr tenv acc expr in
            let _, acc = newCstrs [ TCVar nE; TCTuple(TCVar nL, TCVar nR) ] acc in
            newCstrs [ TCVar nR ] acc
        | ListEmpty -> let nV, acc = newVar acc in newCstrs [ TCSet(TCVar nV) ] acc
        | ListEq(expr1, expr2) ->
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCList(TCVar nV) ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCList(TCVar nV) ] acc in
            newCstrs [ TCBool ] acc
        | ListCons(expr1, expr2) ->
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nV ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCList(TCVar nV) ] acc in
            newCstrs [ TCList(TCVar nV) ] acc
        | ListNth(expr1, expr2) ->
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCList(TCVar nV) ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCNat ] acc in
            newCstrs [ TCVar nV ] acc
        | ListLen(expr) ->
            let nV, acc = newVar acc in
            let n, acc = ofExpr tenv acc expr in
            let _, acc = newCstrs [ TCVar n; TCList(TCVar nV) ] acc in
            newCstrs [ TCNat ] acc
        | SetEmpty -> let nV, acc = newVar acc in newCstrs [ TCSet(TCVar nV) ] acc
        | SetEq(expr1, expr2) ->
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCSet(TCVar nV) ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCSet(TCVar nV) ] acc in
            newCstrs [ TCBool ] acc
        | SetRange(expr1, expr2) ->
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCNat ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCNat ] acc in
            newCstrs [ TCSet TCNat ] acc
        | SetInsert(expr1, expr2) ->
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nV ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCSet(TCVar nV) ] acc in
            newCstrs [ TCSet(TCVar nV) ] acc
        | SetMem(expr1, expr2) ->
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nV ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCSet(TCVar nV) ] acc in
            newCstrs [ TCBool ] acc
        | SetFilter(var, expr1, expr2) ->
            let nV, acc = newVar acc in
            let n1, acc = ofExpr (Map.add var (TCVar nV) tenv) acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nV ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCSet(TCVar nV) ] acc in
            newCstrs [ TCSet(TCVar nV) ] acc
        | SetExists(var, expr1, expr2) ->
            let nV, acc = newVar acc in
            let n1, acc = ofExpr (Map.add var (TCVar nV) tenv) acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nV ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCSet(TCVar nV) ] acc in
            newCstrs [ TCBool ] acc
        | MapEmpty ->
            let nK, acc = newVar acc in
            let nV, acc = newVar acc in
            newCstrs [ TCMap(TCVar nK, TCVar nV) ] acc
        | MapEq(expr1, expr2) ->
            let nK, acc = newVar acc in
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCMap(TCVar nK, TCVar nV) ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCMap(TCVar nK, TCVar nV) ] acc in
            newCstrs [ TCBool ] acc
        | MapAdd(expr1, expr2, expr3) ->
            let nK, acc = newVar acc in
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nK ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCVar nV ] acc in
            let n3, acc = ofExpr tenv acc expr3 in
            let _, acc = newCstrs [ TCVar n3; TCMap(TCVar nK, TCVar nV) ] acc in
            newCstrs [ TCMap(TCVar nK, TCVar nV) ] acc
        | MapFindOpt(expr1, expr2) ->
            let nK, acc = newVar acc in
            let nV, acc = newVar acc in
            let n1, acc = ofExpr tenv acc expr1 in
            let _, acc = newCstrs [ TCVar n1; TCVar nK ] acc in
            let n2, acc = ofExpr tenv acc expr2 in
            let _, acc = newCstrs [ TCVar n2; TCMap(TCVar nK, TCVar nV) ] acc in
            newCstrs [ TCUnion(TCName "option", TCVar nV) ] acc
        | Univ t -> newCstrs [ TCSet(ofType t) ] acc

    ofExpr tenv acc expr
