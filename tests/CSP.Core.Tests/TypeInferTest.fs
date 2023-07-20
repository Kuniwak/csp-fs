module CSP.Core.Tests.TypeCstrTest

open Xunit
open CSP.Core
open CSP.Core.Var
open CSP.Core.Expr
open CSP.Core.Ctor
open CSP.Core.ExprShorthand
open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.TypeInfer

let tcFmt (p: uint * TypeCstr) =
    match p with
    | n, tc -> $"({n}, {format tc})"

type ExprTestCaseOk =
    { Expr: Expr
      Expected: TypeCstr
      Line: string }

let exprTestCasesOk: obj[] list =
    [ [| { Expr = litUnit
           Expected = TCUnit
           Line = __LINE__ } |]
      [| { Expr = litTrue
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = litFalse
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = litNat 0u
           Expected = TCNat
           Line = __LINE__ } |]
      [| { Expr = litEmpty (TSet TUnit)
           Expected = TCSet TCUnit
           Line = __LINE__ } |]
      [| { Expr = litEmpty (TList TUnit)
           Expected = TCList TCUnit
           Line = __LINE__ } |]
      [| { Expr = litEmpty (TMap(TUnit, TUnit))
           Expected = TCMap(TCUnit, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = litError
           Expected = TCError
           Line = __LINE__ } |]
      [| { Expr = ctor "Foo" litUnit
           Expected = TCUnion(UNName "foo", TCUnit)
           Line = __LINE__ } |]
      [| { Expr = throw
           Expected = TCError
           Line = __LINE__ } |]
      [| { Expr = ifExpr litTrue litUnit litUnit
           Expected = TCUnit
           Line = __LINE__ } |]
      [| { Expr = ifExpr litTrue litTrue litFalse
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" litUnit)
                 (Map [ (Ctor "Some", (None, litTrue)); (Ctor "None", (None, litFalse)) ])
                 None
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" litUnit)
                 (Map [ (Ctor "Some", (None, ctor "Some" litUnit)) ])
                 (Some(Some(Var "x"), varRef "x"))
           Expected = TCUnion(UNName "option", TCUnit)
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" litUnit) Map.empty (Some(Some(Var "x"), varRef "x"))
           Expected = TCUnion(UNName "option", TCUnit)
           Line = __LINE__ } |]
      [| { Expr = varRef "GLOBAL"
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = eq (TSet(TNat)) (litEmpty (TSet(TNat))) (litEmpty (TSet(TNat)))
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = not litTrue
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = less (TSet(TNat)) (litEmpty (TSet(TNat))) (litEmpty (TSet(TNat)))
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = plus (TSet(TNat)) (litEmpty (TSet(TNat))) (litEmpty (TSet(TNat)))
           Expected = TCSet TCNat
           Line = __LINE__ } |]
      [| { Expr = minus (TSet(TNat)) (litEmpty (TSet(TNat))) (litEmpty (TSet(TNat)))
           Expected = TCSet TCNat
           Line = __LINE__ } |]
      [| { Expr = times (TSet(TNat)) (litEmpty (TSet(TNat))) (litEmpty (TSet(TNat)))
           Expected = TCSet TCNat
           Line = __LINE__ } |]
      [| { Expr = size (TList TBool) (listCons litTrue (litEmpty (TList TBool)))
           Expected = TCNat
           Line = __LINE__ } |]
      [| { Expr = size (TSet TNat) (litEmpty (TSet TNat))
           Expected = TCNat
           Line = __LINE__ } |]
      [| { Expr = size (TMap(TNat, TNat)) (litEmpty (TMap(TNat, TNat)))
           Expected = TCNat
           Line = __LINE__ } |]
      [| { Expr = filter (TList TUnit) "x" litTrue (litEmpty (TList TUnit))
           Expected = TCList TCUnit
           Line = __LINE__ } |]
      [| { Expr = filter (TSet TUnit) "x" litTrue (litEmpty (TSet TUnit))
           Expected = TCSet TCUnit
           Line = __LINE__ } |]
      [| { Expr = filter (TList TBool) "x" (varRef "x") (litEmpty (TList TBool))
           Expected = TCList TCBool
           Line = __LINE__ } |]
      [| { Expr = exists (TList TUnit) "x" litTrue (litEmpty (TList TUnit))
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = exists (TList TBool) "x" (varRef "x") (litEmpty (TList TBool))
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = tuple (litEmpty (TSet(TNat))) (litEmpty (TSet(TBool)))
           Expected = TCTuple(TCSet(TCNat), TCSet(TCBool))
           Line = __LINE__ } |]
      [| { Expr = tupleFst (tuple litTrue (litNat 0u))
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = tupleSnd (tuple litTrue (litNat 0u))
           Expected = TCNat
           Line = __LINE__ } |]
      [| { Expr = listCons litTrue (litEmpty (TList TBool))
           Expected = TCList TCBool
           Line = __LINE__ } |]
      [| { Expr = listNth (litEmpty (TList TBool)) (litNat 0u)
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = setRange (litNat 0u) (litNat 0u)
           Expected = TCSet TCNat
           Line = __LINE__ } |]
      [| { Expr = setInsert (litNat 0u) (litEmpty (TSet TNat))
           Expected = TCSet TCNat
           Line = __LINE__ } |]
      [| { Expr = setMem (litNat 0u) (litEmpty (TSet TNat))
           Expected = TCBool
           Line = __LINE__ } |]
      [| { Expr = mapAdd (litNat 0u) litUnit (litEmpty (TMap(TNat, TUnit)))
           Expected = TCMap(TCNat, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = mapFindOpt (litNat 0u) (litEmpty (TMap(TNat, TUnit)))
           Expected = TCUnion(UNName "option", TCUnit)
           Line = __LINE__ } |]
      [| { Expr = univ (TMap(TNat, TUnit))
           Expected = TCSet(TCMap(TCNat, TCUnit))
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof exprTestCasesOk)>]
let inferExprOk (tc: ExprTestCaseOk) =
    let optionCm = [ ("option", Ctor "Some", TVar 0u); ("option", Ctor "None", TUnit) ] in
    let fooCm = [ ("foo", Ctor "Foo", TUnit) ] in
    let cm = CtorMap.from (optionCm @ fooCm) in
    let tenv = TypeEnv.from [ ("GLOBAL", TCBool) ] in

    match infer cm 0u Map.empty tenv tc.Expr with
    | Ok(actual, _, _) ->
        Assert.True(
            tc.Expected = typeCstr actual,
            $"line at {tc.Line}\n\nExpected: {format tc.Expected}\nActual:   {format (typeCstr actual)}\ninferred as:\n{Expr.format actual}"
        )
    | Error terr -> Assert.Fail $"line at {tc.Line}\n\n{formatTypeError tc.Expr terr}"

type ExprTestCaseError =
    { Expr: Expr
      Expected: TypeError
      Line: string }

let exprTestCasesError: obj[] list =
    [ [| { Expr = ctor "UndefinedCtor" litUnit
           Expected = NoSuchCtor(Ctor "UndefinedCtor")
           Line = __LINE__ } |]
      [| { Expr = ctor "Foo" litTrue
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = ifExpr litUnit litUnit litFalse
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = ifExpr litTrue litUnit litFalse
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = matchExpr litUnit Map.empty None
           Expected = TypeMismatch(TCUnit, TCUnion(UNAny, TCVar 0u))
           Line = __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" litUnit)
                 (Map [ (Ctor "Some", (None, litTrue)); (Ctor "None", (None, litUnit)) ])
                 None
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" litUnit) (Map [ (Ctor "Some", (None, litTrue)) ]) (Some(None, litUnit))
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Foo" litUnit) Map.empty None
           Expected = EmptyMatch
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Foo" litUnit) Map.empty None
           Expected = EmptyMatch
           Line = __LINE__ } |]
      [| { Expr = varRef "undefined"
           Expected = UnboundVariable(Var "undefined")
           Line = __LINE__ } |]
      [| { Expr = eq TBool litFalse litUnit
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = eq TBool litUnit litFalse
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = eq TBool litUnit litUnit
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = less TBool litFalse litUnit
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = less TBool litUnit litFalse
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = less TBool litUnit litUnit
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = not litUnit
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = plus TUnit litUnit litUnit
           Expected = TypeNotDerived(TUnit, "Ring")
           Line = __LINE__ } |]
      [| { Expr = plus TBool litUnit litTrue
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = plus TBool litTrue litUnit
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = minus TUnit litUnit litUnit
           Expected = TypeNotDerived(TUnit, "Field")
           Line = __LINE__ } |]
      [| { Expr = minus TBool litUnit litTrue
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = minus TBool litTrue litUnit
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = times TUnit litUnit litUnit
           Expected = TypeNotDerived(TUnit, "Ring")
           Line = __LINE__ } |]
      [| { Expr = times TBool litUnit litTrue
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = times TBool litTrue litUnit
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = size TUnit litUnit
           Expected = TypeNotDerived(TUnit, "Size")
           Line = __LINE__ } |]
      [| { Expr = size (TSet TUnit) litUnit
           Expected = TypeMismatch(TCUnit, TCSet(TCUnit))
           Line = __LINE__ } |]
      [| { Expr = size (TSet TUnit) (litEmpty (TSet TBool))
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = filter TUnit "x" litTrue litUnit
           Expected = TypeNotDerived(TUnit, "Enum")
           Line = __LINE__ } |]
      [| { Expr = filter (TSet TUnit) "x" litTrue (litEmpty (TSet TBool))
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = filter (TSet TUnit) "x" litUnit (litEmpty (TSet TBool))
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = exists TUnit "x" litTrue litUnit
           Expected = TypeNotDerived(TUnit, "Enum")
           Line = __LINE__ } |]
      [| { Expr = exists (TSet TUnit) "x" litTrue (litEmpty (TSet TBool))
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = exists (TSet TUnit) "x" litUnit (litEmpty (TSet TBool))
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = tupleFst litUnit
           Expected = TypeMismatch(TCUnit, TCTuple(TCVar 0u, TCVar 1u))
           Line = __LINE__ } |]
      [| { Expr = tupleSnd litUnit
           Expected = TypeMismatch(TCUnit, TCTuple(TCVar 0u, TCVar 1u))
           Line = __LINE__ } |]
      [| { Expr = listCons litUnit litUnit
           Expected = TypeMismatch(TCUnit, TCList TCUnit)
           Line = __LINE__ } |]
      [| { Expr = listCons litUnit (litEmpty (TList(TBool)))
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = listNth litUnit (litNat 0u)
           Expected = TypeMismatch(TCUnit, TCList(TCVar 0u))
           Line = __LINE__ } |]
      [| { Expr = listNth (litEmpty (TList(TBool))) litUnit
           Expected = TypeMismatch(TCUnit, TCNat)
           Line = __LINE__ } |]
      [| { Expr = setRange litUnit (litNat 0u)
           Expected = TypeMismatch(TCUnit, TCNat)
           Line = __LINE__ } |]
      [| { Expr = setRange (litNat 0u) litUnit
           Expected = TypeMismatch(TCUnit, TCNat)
           Line = __LINE__ } |]
      [| { Expr = setInsert litUnit litUnit
           Expected = TypeMismatch(TCUnit, TCSet(TCUnit))
           Line = __LINE__ } |]
      [| { Expr = setInsert litTrue (litEmpty (TSet(TUnit)))
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = setMem litUnit litUnit
           Expected = TypeMismatch(TCUnit, TCSet(TCUnit))
           Line = __LINE__ } |]
      [| { Expr = setMem litTrue (litEmpty (TSet(TUnit)))
           Expected = TypeMismatch(TCUnit, TCBool)
           Line = __LINE__ } |]
      [| { Expr = mapAdd litUnit litUnit litUnit
           Expected = TypeMismatch(TCUnit, TCMap(TCUnit, TCUnit))
           Line = __LINE__ } |]
      [| { Expr = mapAdd litUnit litTrue (litEmpty (TMap(TBool, TBool)))
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = mapAdd litTrue litUnit (litEmpty (TMap(TBool, TBool)))
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |]
      [| { Expr = mapFindOpt litUnit litUnit
           Expected = TypeMismatch(TCUnit, TCMap(TCUnit, TCVar 0u))
           Line = __LINE__ } |]
      [| { Expr = mapFindOpt litUnit (litEmpty (TMap(TBool, TBool)))
           Expected = TypeMismatch(TCBool, TCUnit)
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof exprTestCasesError)>]
let inferExprError (tc: ExprTestCaseError) =
    let optionCm = [ ("option", Ctor "Some", TVar 0u); ("option", Ctor "None", TUnit) ] in
    let fooCm = [ ("foo", Ctor "Foo", TUnit) ] in
    let cm = CtorMap.from (optionCm @ fooCm) in
    let tenv = TypeEnv.from [ ("GLOBAL", TCBool) ] in

    match infer cm 0u Map.empty tenv tc.Expr with
    | Ok(actual, _, _) -> Assert.Fail $"line at {line}\n\n{Expr.format actual}"
    | Error terr ->
        let actual = unwrapTypeError terr in

        Assert.True(
            tc.Expected = actual,
            $"line at {tc.Line}\nExpected: %s{formatTypeError tc.Expr tc.Expected}\nActual:   %s{formatTypeError tc.Expr actual}"
        )
