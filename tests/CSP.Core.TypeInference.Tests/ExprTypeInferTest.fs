module CSP.Core.Tests.ExprTypeInferTest

open Xunit
open CSP.Core.Util
open CSP.Core
open CSP.Core.Var
open CSP.Core.Expr
open CSP.Core.Ctor
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.TypeCstrShorthand
open CSP.Core.TypeEnvError
open CSP.Core.TypeError
open CSP.Core.TypeInference
open CSP.Core.ExprTypeInference

type ExprTestCaseOk =
    { Expr: Expr<unit>
      Expected: Type.Type
      Line: string }

let exprTestCasesOk: obj[] list =
    [ [| { Expr = litUnit
           Expected = tUnit
           Line = __LINE__ } |]
      [| { Expr = litTrue
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = litFalse
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = litNat 0u
           Expected = tNat
           Line = __LINE__ } |]
      [| { Expr = litEmpty (tSet tUnit)
           Expected = tSet tUnit
           Line = __LINE__ } |]
      [| { Expr = litEmpty (tList tUnit)
           Expected = tList tUnit
           Line = __LINE__ } |]
      [| { Expr = litEmpty (tMap tBool tUnit)
           Expected = tMap tBool tUnit
           Line = __LINE__ } |]
      [| { Expr = ctor "Foo" []
           Expected = tUnion "foo" [ ("Foo", []) ]
           Line = __LINE__ } |]
      [| { Expr = ifExpr litTrue litUnit litUnit
           Expected = tUnit
           Line = __LINE__ } |]
      [| { Expr = ifExpr litTrue litTrue litFalse
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" [ litUnit ]) [ (Some("Some"), [ "x" ], litTrue); (Some("None"), [], litFalse) ]
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litUnit ])
                 [ (Some("Some"), [ "x" ], ctor "Some" [ varRef "x" ])
                   (None, [ "x" ], varRef "x") ]
           Expected = tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ]
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" [ litUnit ]) [ (None, [ "x" ], varRef "x") ]
           Expected = tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ]
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" [ litUnit ]) [ (None, [ "_" ], litTrue) ]
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = varRef "GLOBAL"
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = eq (tSet tNat) (litEmpty (tSet tNat)) (litEmpty (tSet tNat))
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = boolNot litTrue
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = less (tSet tNat) (litEmpty (tSet tNat)) (litEmpty (tSet tNat))
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = plus (tSet tNat) (litEmpty (tSet tNat)) (litEmpty (tSet tNat))
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Expr = minus (tSet tNat) (litEmpty (tSet tNat)) (litEmpty (tSet tNat))
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Expr = times (tSet tNat) (litEmpty (tSet tNat)) (litEmpty (tSet tNat))
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Expr = size (tList tBool) (listCons litTrue (litEmpty (tList tBool)))
           Expected = tNat
           Line = __LINE__ } |]
      [| { Expr = size (tSet tNat) (litEmpty (tSet tNat))
           Expected = tNat
           Line = __LINE__ } |]
      [| { Expr = size (tMap tNat tNat) (litEmpty (tMap tNat tNat))
           Expected = tNat
           Line = __LINE__ } |]
      [| { Expr = filter (tList tUnit) "x" litTrue (litEmpty (tList tUnit))
           Expected = tList tUnit
           Line = __LINE__ } |]
      [| { Expr = filter (tSet tUnit) "x" litTrue (litEmpty (tSet tUnit))
           Expected = tSet tUnit
           Line = __LINE__ } |]
      [| { Expr = filter (tList tBool) "x" (varRef "x") (litEmpty (tList tBool))
           Expected = tList tBool
           Line = __LINE__ } |]
      [| { Expr = exists (tList tUnit) "x" litTrue (litEmpty (tList tUnit))
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = exists (tList tBool) "x" (varRef "x") (litEmpty (tList tBool))
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = tuple2 (litNat 0u) litTrue
           Expected = tTuple2 tNat tBool
           Line = __LINE__ } |]
      [| { Expr = tupleFst (tuple2 litTrue (litNat 0u))
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = tupleSnd (tuple2 litTrue (litNat 0u))
           Expected = tNat
           Line = __LINE__ } |]
      [| { Expr = listCons litTrue (litEmpty (tList tBool))
           Expected = tList tBool
           Line = __LINE__ } |]
      [| { Expr = listNth (litEmpty (tList tBool)) (litNat 0u)
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = setRange (litNat 0u) (litNat 0u)
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Expr = setInsert (litNat 0u) (litEmpty (tSet tNat))
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Expr = setMem (litNat 0u) (litEmpty (tSet tNat))
           Expected = tBool
           Line = __LINE__ } |]
      [| { Expr = mapAdd (litNat 0u) litUnit (litEmpty (tMap tNat tUnit))
           Expected = tMap tNat tUnit
           Line = __LINE__ } |]
      [| { Expr = mapFindOpt (litNat 0u) (litEmpty (tMap tNat tUnit))
           Expected = tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ]
           Line = __LINE__ } |]
      [| { Expr = univ (tMap tNat tUnit)
           Expected = tSet (tMap tNat tUnit)
           Line = __LINE__ } |]
      [| { Expr = tuple2 (ctor "Some" [ litUnit ]) (ctor "Some" [ litTrue ])
           Expected =
             tTuple2
                 (tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ])
                 (tUnion "option" [ ("Some", [ tBool ]); ("None", []) ])
           Line = __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litTrue ])
                 [ (Some("Some"), [ "x" ], boolNot (varRef "x")); (Some("None"), [], litTrue) ]
           Expected = tBool
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof exprTestCasesOk)>]
let inferExprOk (tc: ExprTestCaseOk) =
    let tOption = tUnion "option" [ ("Some", [ tVar 0u ]); ("None", []) ] in
    let tFoo = tUnion "foo" [ ("Foo", []) ] in
    let cm = CtorMap.from [ tOption; tFoo ] in
    let tenv, s = ResultEx.get TypeEnvError.format (from [ ("GLOBAL", tBool) ]) in
    
    match postProcess (infer cm tenv tc.Expr s) with
    | Ok(actual, s) ->
        Assert.True(
            tc.Expected = get actual,
            $"""line %s{tc.Line}

Expected: %s{Type.format tc.Expected}
Actual:   %s{Type.format (get actual)}
Inferred as:
%s{Expr.format typeAnnotation actual}

TVar mapping:
%s{TypeCstrUncertainVar.format s.UncertainVarMap}
"""
        )
    | Error terr ->
        Assert.Fail
            $"""line %s{tc.Line}

Expr:
%s{Expr.format noAnnotation tc.Expr}

Expected: (no error)
Actual:   %s{format terr}
"""


type ExprTestCaseError =
    { Expr: Expr<unit>
      Expected: TypeError
      Line: string }

let exprTestCasesError: obj[] list =
    [ [| { Expr = ctor "UndefinedCtor" []
           Expected = NoSuchCtor(Ctor "UndefinedCtor")
           Line = __LINE__ } |]
      [| { Expr = ctor "Foo" [ litTrue ]
           Expected = UnionValueLenMismatch(Ctor "Foo", 1, 0)
           Line = __LINE__ } |]
      [| { Expr = ifExpr litUnit litUnit litFalse
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = ifExpr litTrue litUnit litFalse
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = matchExpr litUnit [ (None, [], litTrue) ]
           Expected = NotUnion(tcUnit)
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" [ litUnit ]) [ (Some("Some"), [ "_" ], litTrue); (Some("None"), [], litUnit) ]
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" [ litUnit ]) [ (Some("Some"), [ "_" ], litTrue); (None, [ "_" ], litUnit) ]
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" [ litUnit ]) [ (Some("Some"), [ "x" ], varRef "x"); (None, [ "_" ], litTrue) ]
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Foo" []) []
           Expected = EmptyMatch
           Line = __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" [ litUnit ]) [ (None, [ "x"; "y" ], litUnit) ]
           Expected = DefaultClauseArgumentsLenMustBe1([ Some(Var "x"); Some(Var "y") ])
           Line = __LINE__ } |]
      [| { Expr = varRef "undefined"
           Expected = TypeEnvError(UnboundVariable(Var "undefined"))
           Line = __LINE__ } |]
      [| { Expr = eq tBool litFalse litUnit
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = eq tBool litUnit litFalse
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = eq tBool litUnit litUnit
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = less tNat litFalse (litNat 0u)
           Expected = TypeMismatch(Set [tcBool; tcNat])
           Line = __LINE__ } |]
      [| { Expr = less tNat (litNat 0u) litFalse
           Expected = TypeMismatch(Set [tcNat; tcBool])
           Line = __LINE__ } |]
      [| { Expr = less tBool litTrue litFalse
           Expected = TypeNotDerived(tBool, ClassOrd.name)
           Line = __LINE__ } |]
      [| { Expr = boolNot litUnit
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = plus tUnit litUnit litUnit
           Expected = TypeNotDerived(tUnit, ClassPlus.name)
           Line = __LINE__ } |]
      [| { Expr = plus tBool litUnit litTrue
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = plus tBool litTrue litUnit
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = minus tUnit litUnit litUnit
           Expected = TypeNotDerived(tUnit, ClassMinus.name)
           Line = __LINE__ } |]
      [| { Expr = minus tNat litUnit (litNat 0u)
           Expected = TypeMismatch(Set [tcUnit; tcNat])
           Line = __LINE__ } |]
      [| { Expr = minus tNat (litNat 0u) litUnit
           Expected = TypeMismatch(Set [tcNat; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = times tUnit litUnit litUnit
           Expected = TypeNotDerived(tUnit, ClassTimes.name)
           Line = __LINE__ } |]
      [| { Expr = times tBool litUnit litTrue
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = times tBool litTrue litUnit
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = size tUnit litUnit
           Expected = TypeNotDerived(tUnit, ClassSize.name)
           Line = __LINE__ } |]
      [| { Expr = size (tSet tUnit) litUnit
           Expected = TypeMismatch(Set [tcSet tcUnit; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = size (tSet tUnit) (litEmpty (tSet tBool))
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = filter tUnit "x" litTrue litUnit
           Expected = TypeNotDerived(tUnit, ClassEnum.name)
           Line = __LINE__ } |]
      [| { Expr = filter (tSet tUnit) "x" litTrue (litEmpty (tSet tBool))
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = filter (tSet tUnit) "x" litUnit (litEmpty (tSet tBool))
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = exists tUnit "x" litTrue litUnit
           Expected = TypeNotDerived(tUnit, ClassEnum.name)
           Line = __LINE__ } |]
      [| { Expr = exists (tSet tUnit) "x" litTrue (litEmpty (tSet tBool))
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = exists (tSet tUnit) "x" litUnit (litEmpty (tSet tBool))
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = tupleFst litUnit
           Expected = TupleIndexOutOfBounds(tcUnit, 0u)
           Line = __LINE__ } |]
      [| { Expr = tupleFst litTrue
           Expected = NotTuple(tcBool)
           Line = __LINE__ } |]
      [| { Expr = tupleSnd litUnit
           Expected = TupleIndexOutOfBounds(tcUnit, 1u)
           Line = __LINE__ } |]
      [| { Expr = tupleSnd litTrue
           Expected = NotTuple(tcBool)
           Line = __LINE__ } |]
      [| { Expr = listCons litUnit litUnit
           Expected = TypeMismatch(Set [tcUnit; tcList tcUnit])
           Line = __LINE__ } |]
      [| { Expr = listCons litUnit (litEmpty (tList tBool))
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = listNth litUnit (litNat 0u)
           Expected = TypeMismatch(Set [tcUnit; tcList (tcUncertain 0u)])
           Line = __LINE__ } |]
      [| { Expr = listNth (litEmpty (tList tBool)) litUnit
           Expected = TypeMismatch(Set [tcUnit; tcNat])
           Line = __LINE__ } |]
      [| { Expr = setRange litUnit (litNat 0u)
           Expected = TypeMismatch(Set [tcUnit; tcNat])
           Line = __LINE__ } |]
      [| { Expr = setRange (litNat 0u) litUnit
           Expected = TypeMismatch(Set [tcUnit; tcNat])
           Line = __LINE__ } |]
      [| { Expr = setInsert litUnit litUnit
           Expected = TypeMismatch(Set [tcUnit; tcSet tcUnit])
           Line = __LINE__ } |]
      [| { Expr = setInsert litTrue (litEmpty (tSet tUnit))
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = setMem litUnit litUnit
           Expected = TypeMismatch(Set [tcUnit; tcSet tcUnit])
           Line = __LINE__ } |]
      [| { Expr = setMem litTrue (litEmpty (tSet tUnit))
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |]
      [| { Expr = mapAdd litUnit litUnit litUnit
           Expected = TypeMismatch(Set [tcUnit; tcMap tcUnit tcUnit])
           Line = __LINE__ } |]
      [| { Expr = mapAdd litUnit litTrue (litEmpty (tMap tBool tBool))
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = mapAdd litTrue litUnit (litEmpty (tMap tBool tBool))
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr = mapFindOpt litUnit litUnit
           Expected = TypeMismatch(Set [tcUnit; tcMap tcUnit (tcUncertain 0u)])
           Line = __LINE__ } |]
      [| { Expr = mapFindOpt litUnit (litEmpty (tMap tBool tBool))
           Expected = TypeMismatch(Set [tcBool; tcUnit])
           Line = __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litUnit ])
                 [ (Some("Some"), [ "x" ], boolNot (varRef "x")); (Some("None"), [], litTrue) ]
           Expected = TypeMismatch(Set [tcUnit; tcBool])
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof exprTestCasesError)>]
let inferExprError (tc: ExprTestCaseError) =
    let tOption = tUnion "option" [ ("Some", [ tVar 0u ]); ("None", []) ] in
    let tFoo = tUnion "foo" [ ("Foo", []) ] in
    let cm = CtorMap.from [ tOption; tFoo ] in
    let tenv, s = ResultEx.get TypeEnvError.format (from [ ("GLOBAL", tBool) ]) in

    match postProcess (infer cm tenv tc.Expr s) with
    | Ok(actual, s) ->
        Assert.Fail
            $"""line %s{tc.Line}

Inferred as:
%s{Expr.format typeAnnotation actual}

TVar mapping:
%s{TypeCstrUncertainVar.format s.UncertainVarMap}
"""
    | Error terr ->
        let actual = unwrapTypeError terr in

        Assert.True(
            tc.Expected = actual,
            $"""line %s{tc.Line}

Expected: %s{format tc.Expected}
Actual:   %s{format actual}"""
        )