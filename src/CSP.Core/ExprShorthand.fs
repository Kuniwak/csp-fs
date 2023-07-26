module CSP.Core.ExprShorthand

open CSP.Core.Ctor
open CSP.Core.LineNum
open CSP.Core.Expr
open CSP.Core.Var

let litTrue = LitTrue((), unknown)
let litFalse = LitFalse((), unknown)
let litNat n = LitNat(n, (), unknown)
let litEmpty t = LitEmpty(t, (), unknown)
let ctor ctor expr = Union(Ctor ctor, expr, (), unknown)

let ifExpr exprCond exprThen exprElse =
    If(exprCond, exprThen, exprElse, (), unknown)

let matchExpr exprUnion exprMap =
    Match(
        exprUnion,
        Map
            [ for ctorOpt, vars, expr in exprMap ->
                  (Option.map Ctor ctorOpt, (List.map (fun v -> if v = "_" then None else Some(Var v)) vars, expr)) ],
        (),
        unknown
    )

let varRef var = VarRef(Var var, (), unknown)
let eq t expr1 expr2 = Eq(t, expr1, expr2, (), unknown)
let boolNot expr = BoolNot(expr, (), unknown)
let less t expr1 expr2 = Less(t, expr1, expr2, (), unknown)
let plus t expr1 expr2 = Plus(t, expr1, expr2, (), unknown)
let minus t expr1 expr2 = Minus(t, expr1, expr2, (), unknown)
let times t expr1 expr2 = Times(t, expr1, expr2, (), unknown)
let size t expr = Size(t, expr, (), unknown)

let filter t var expr1 expr2 =
    Filter(t, Var var, expr1, expr2, (), unknown)

let exists t var expr1 expr2 =
    Exists(t, Var var, expr1, expr2, (), unknown)

let litUnit = Tuple([], (), unknown)
let tuple exprs = Tuple(exprs, (), unknown)
let tuple2 expr1 expr2 = Tuple([ expr1; expr2 ], (), unknown)

let tuple3 expr1 expr2 expr3 =
    Tuple([ expr1; expr2; expr3 ], (), unknown)

let tupleNth expr idx = TupleNth(expr, idx, (), unknown)
let tupleFst expr = TupleNth(expr, 0u, (), unknown)
let tupleSnd expr = TupleNth(expr, 1u, (), unknown)

let listCons exprElem exprList =
    ListCons(exprElem, exprList, (), unknown)

let listNth expr exprIdx = ListNth(expr, exprIdx, (), unknown)

let setRange exprLower exprUpper =
    SetRange(exprLower, exprUpper, (), unknown)

let setInsert exprElem exprSet =
    SetInsert(exprElem, exprSet, (), unknown)

let setMem exprElem exprSet = SetMem(exprElem, exprSet, (), unknown)

let mapAdd exprKey exprVal exprMap =
    MapAdd(exprKey, exprVal, exprMap, (), unknown)

let mapFindOpt exprKey exprMap =
    MapFindOpt(exprKey, exprMap, (), unknown)

let univ t = Univ(t, (), unknown)
