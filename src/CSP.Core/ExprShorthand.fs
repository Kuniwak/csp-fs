module CSP.Core.ExprShorthand

open CSP.Core.Ctor
open CSP.Core.Expr
open CSP.Core.Var

let litTrue line = LitTrue((), line)
let litFalse line = LitFalse((), line)
let litNat n line = LitNat(n, (), line)
let litEmpty t line = LitEmpty(t, (), line)
let ctor ctor expr line = Union(Ctor ctor, expr, (), line)

let ifExpr exprCond exprThen exprElse line =
    If(exprCond, exprThen, exprElse, (), line)

let matchExpr exprUnion exprMap line =
    Match(
        exprUnion,
        Map
            [ for (ctor, vars), expr in exprMap ->
                  ((if ctor = "_" then None else Some(Ctor ctor)),
                   (vars |> List.map (fun v -> if v = "_" then None else Some(Var v)), expr)) ],
        (),
        line
    )

let varRef var line = VarRef(Var var, (), line)
let eq t expr1 expr2 line = Eq(t, expr1, expr2, (), line)
let boolNot expr line = BoolNot(expr, (), line)
let less t expr1 expr2 line = Less(t, expr1, expr2, (), line)
let plus t expr1 expr2 line = Plus(t, expr1, expr2, (), line)
let minus t expr1 expr2 line = Minus(t, expr1, expr2, (), line)
let times t expr1 expr2 line = Times(t, expr1, expr2, (), line)
let size t expr line = Size(t, expr, (), line)

let filter t var expr1 expr2 line =
    Filter(t, Var var, expr1, expr2, (), line)

let exists t var expr1 expr2 line =
    Exists(t, Var var, expr1, expr2, (), line)

let litUnit line = LitUnit((), line)
let tuple2 expr1 expr2 line = Tuple(expr1, expr2, (), line)

let tuple3 expr1 expr2 expr3 line =
    Tuple(expr1, Tuple(expr2, expr3, (), line), (), line)

let tupleFst expr line = TupleFst(expr, (), line)
let tupleSnd expr line = TupleSnd(expr, (), line)

let listCons exprElem exprList line = ListCons(exprElem, exprList, (), line)

let listContains exprElem exprList line =
    ListContains(exprElem, exprList, (), line)

let listNth expr exprIdx line = ListNth(expr, exprIdx, (), line)

let setRange exprLower exprUpper line =
    SetRange(exprLower, exprUpper, (), line)

let setInsert exprElem exprSet line = SetInsert(exprElem, exprSet, (), line)
let setRemove exprElem exprSet line = SetRemove(exprElem, exprSet, (), line)

let setMem exprElem exprSet line = SetMem(exprElem, exprSet, (), line)

let mapAdd exprKey exprVal exprMap line =
    MapAdd(exprKey, exprVal, exprMap, (), line)

let mapFindOpt exprKey exprMap line = MapFindOpt(exprKey, exprMap, (), line)

let univ t line = Univ(t, (), line)
