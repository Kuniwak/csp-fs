module CSP.Core.ExprShorthand

open CSP.Core.Ctor
open CSP.Core.LineNum
open CSP.Core.Expr
open CSP.Core.Var

let litUnit = LitUnit (None, unknown)
let litTrue = LitTrue (None, unknown)
let litFalse = LitFalse (None, unknown)
let litNat n = LitNat (n, None, unknown)
let litEmpty t = LitEmpty(t, None, unknown)
let litError = LitError(None, unknown)
let ctor ctor expr = Union(Ctor ctor, expr, None, unknown)
let throw = Throw(None, unknown)
let ifExpr exprCond exprThen exprElse = If(exprCond, exprThen, exprElse, None, unknown)
let matchExpr exprUnion exprMap exprDef = Match(exprUnion, exprMap, exprDef, None, unknown)
let varRef var = VarRef(Var var, None, unknown)
let eq t expr1 expr2 = Eq(t, expr1, expr2, None, unknown)
let not expr = Not(expr, None, unknown)
let less t expr1 expr2 = Less(t, expr1, expr2, None, unknown)
let plus t expr1 expr2 = Plus(t, expr1, expr2, None, unknown)
let minus t expr1 expr2 = Minus(t, expr1, expr2, None, unknown)
let times t expr1 expr2 = Times(t, expr1, expr2, None, unknown)
let size t expr = Size(t, expr, None, unknown)
let filter t var expr1 expr2 = Filter(t, Var var, expr1, expr2, None, unknown)
let exists t var expr1 expr2 = Exists(t, Var var, expr1, expr2, None, unknown)
let tuple expr1 expr2 = Tuple(expr1, expr2, None, unknown)
let tupleFst expr = TupleFst(expr, None, unknown)
let tupleSnd expr = TupleSnd(expr, None, unknown)
let listCons exprElem exprList = ListCons(exprElem, exprList, None, unknown)
let listNth exprList exprIdx = ListNth(exprList, exprIdx, None, unknown)
let setRange exprLower exprUpper = SetRange(exprLower, exprUpper, None, unknown)
let setInsert exprElem exprSet = SetInsert(exprElem, exprSet, None, unknown)
let setMem exprElem exprSet = SetMem(exprElem, exprSet, None, unknown)
let mapAdd exprKey exprVal exprMap = MapAdd(exprKey, exprVal, exprMap, None, unknown)
let mapFindOpt exprKey exprMap = MapFindOpt(exprKey, exprMap, None, unknown)
let univ t = Univ(t, None, unknown)

