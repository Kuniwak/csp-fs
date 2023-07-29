module CSP.Core.ExprTypeInferenceTest

open Xunit
open CSP.Core
open CSP.Core.Type
open CSP.Core.Var
open CSP.Core.Expr
open CSP.Core.Ctor
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.TypeCstrShorthand
open CSP.Core.TypeEnvError
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.ExprTypeInference

type ExprTestCaseOk = { Expr: Expr<unit>; Expected: Type }

let exprTestCasesOk: obj[] list =
    [ [| { Expr = litUnit __LINE__
           Expected = tUnit __LINE__ } |]
      [| { Expr = litTrue __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr = litFalse __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr = litNat 0u __LINE__
           Expected = tNat __LINE__ } |]
      [| { Expr = litEmpty (tSet (tUnit __LINE__) __LINE__) __LINE__
           Expected = tSet (tUnit __LINE__) __LINE__ } |]
      [| { Expr = litEmpty (tList (tUnit __LINE__) __LINE__) __LINE__
           Expected = tList (tUnit __LINE__) __LINE__ } |]
      [| { Expr = litEmpty (tMap (tBool __LINE__) (tUnit __LINE__) __LINE__) __LINE__
           Expected = tMap (tBool __LINE__) (tUnit __LINE__) __LINE__ } |]
      [| { Expr = ctor "Foo" [] __LINE__
           Expected = tUnion "foo" [ ("Foo", []) ] __LINE__ } |]
      [| { Expr = ifExpr (litTrue __LINE__) (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = (tUnit __LINE__) } |]
      [| { Expr = ifExpr (litTrue __LINE__) (litTrue __LINE__) (litFalse __LINE__) __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litUnit __LINE__ ] __LINE__)
                 [ (Some("Some"), [ "x" ], litTrue __LINE__)
                   (Some("None"), [], (litFalse __LINE__)) ]
                 __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litUnit __LINE__ ] __LINE__)
                 [ (Some("Some"), [ "x" ], ctor "Some" [ varRef "x" __LINE__ ] __LINE__)
                   (None, [ "x" ], varRef "x" __LINE__) ]
                 __LINE__
           Expected = tUnion "option" [ ("Some", [ tUnit __LINE__ ]); ("None", []) ] __LINE__ } |]
      [| { Expr =
             matchExpr (ctor "Some" [ litUnit __LINE__ ] __LINE__) [ (None, [ "x" ], varRef "x" __LINE__) ] __LINE__
           Expected = tUnion "option" [ ("Some", [ tUnit __LINE__ ]); ("None", []) ] __LINE__ } |]
      [| { Expr = matchExpr (ctor "Some" [ litUnit __LINE__ ] __LINE__) [ (None, [ "_" ], litTrue __LINE__) ] __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr = varRef "GLOBAL" __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr =
             eq
                 (tSet (tNat __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr = boolNot (litTrue __LINE__) __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr =
             less
                 (tSet (tNat __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr =
             plus
                 (tSet (tNat __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tSet (tNat __LINE__) __LINE__ } |]
      [| { Expr =
             minus
                 (tSet (tNat __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tSet (tNat __LINE__) __LINE__ } |]
      [| { Expr =
             times
                 (tSet (tNat __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tSet (tNat __LINE__) __LINE__ } |]
      [| { Expr =
             size
                 (tList (tBool __LINE__) __LINE__)
                 (listCons (litTrue __LINE__) (litEmpty (tList (tBool __LINE__) __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tNat __LINE__ } |]
      [| { Expr = size (tSet (tNat __LINE__) __LINE__) (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__) __LINE__
           Expected = tNat __LINE__ } |]
      [| { Expr =
             size
                 (tMap (tNat __LINE__) (tNat __LINE__) __LINE__)
                 (litEmpty (tMap (tNat __LINE__) (tNat __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tNat __LINE__ } |]
      [| { Expr =
             filter
                 (tList (tUnit __LINE__) __LINE__)
                 "x"
                 (litTrue __LINE__)
                 (litEmpty (tList (tUnit __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tList (tUnit __LINE__) __LINE__ } |]
      [| { Expr =
             filter
                 (tSet (tUnit __LINE__) __LINE__)
                 "x"
                 (litTrue __LINE__)
                 (litEmpty (tSet (tUnit __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tSet (tUnit __LINE__) __LINE__ } |]
      [| { Expr =
             filter
                 (tList (tBool __LINE__) __LINE__)
                 "x"
                 (varRef "x" __LINE__)
                 (litEmpty (tList (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tList (tBool __LINE__) __LINE__ } |]
      [| { Expr =
             exists
                 (tList (tUnit __LINE__) __LINE__)
                 "x"
                 (litTrue __LINE__)
                 (litEmpty (tList (tUnit __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr =
             exists
                 (tList (tBool __LINE__) __LINE__)
                 "x"
                 (varRef "x" __LINE__)
                 (litEmpty (tList (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr = tuple2 (litNat 0u __LINE__) (litTrue __LINE__) __LINE__
           Expected = tTuple2 (tNat __LINE__) (tBool __LINE__) __LINE__ } |]
      [| { Expr = tupleFst (tuple2 (litTrue __LINE__) (litNat 0u __LINE__) __LINE__) __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr = tupleSnd (tuple2 (litTrue __LINE__) (litNat 0u __LINE__) __LINE__) __LINE__
           Expected = tNat __LINE__ } |]
      [| { Expr = listCons (litTrue __LINE__) (litEmpty (tList (tBool __LINE__) __LINE__) __LINE__) __LINE__
           Expected = tList (tBool __LINE__) __LINE__ } |]
      [| { Expr = listNth (litEmpty (tList (tBool __LINE__) __LINE__) __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr = setRange (litNat 0u __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = tSet (tNat __LINE__) __LINE__ } |]
      [| { Expr = setInsert (litNat 0u __LINE__) (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__) __LINE__
           Expected = tSet (tNat __LINE__) __LINE__ } |]
      [| { Expr = setMem (litNat 0u __LINE__) (litEmpty (tSet (tNat __LINE__) __LINE__) __LINE__) __LINE__
           Expected = tBool __LINE__ } |]
      [| { Expr =
             mapAdd
                 (litNat 0u __LINE__)
                 (litUnit __LINE__)
                 (litEmpty (tMap (tNat __LINE__) (tUnit __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tMap (tNat __LINE__) (tUnit __LINE__) __LINE__ } |]
      [| { Expr =
             mapFindOpt
                 (litNat 0u __LINE__)
                 (litEmpty (tMap (tNat __LINE__) (tUnit __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = tUnion "option" [ ("Some", [ tUnit __LINE__ ]); ("None", []) ] __LINE__ } |]
      [| { Expr = univ (tMap (tNat __LINE__) (tUnit __LINE__) __LINE__) __LINE__
           Expected = tSet (tMap (tNat __LINE__) (tUnit __LINE__) __LINE__) __LINE__ } |]
      [| { Expr =
             tuple2 (ctor "Some" [ litUnit __LINE__ ] __LINE__) (ctor "Some" [ litTrue __LINE__ ] __LINE__) __LINE__
           Expected =
             tTuple2
                 (tUnion "option" [ ("Some", [ tUnit __LINE__ ]); ("None", []) ] __LINE__)
                 (tUnion "option" [ ("Some", [ tBool __LINE__ ]); ("None", []) ] __LINE__)
                 __LINE__ } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litTrue __LINE__ ] __LINE__)
                 [ (Some("Some"), [ "x" ], boolNot (varRef "x" __LINE__) __LINE__)
                   (Some("None"), [], litTrue __LINE__) ]
                 __LINE__
           Expected = tBool __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof exprTestCasesOk)>]
let inferExprOk (tc: ExprTestCaseOk) =
    let tOption =
        tUnion "option" [ ("Some", [ tVar 0u __LINE__ ]); ("None", []) ] __LINE__ in

    let tFoo = tUnion "foo" [ ("Foo", []) ] __LINE__ in
    let cm = CtorMap.from [ tOption; tFoo ] in
    let tenv = TypeCstrEnv.from [ ("GLOBAL", tcBool) ] in

    match postProcess (infer cm tenv tc.Expr init) with
    | Ok(actual, s) ->
        Assert.True(
            tc.Expected = get actual,
            $"""
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
            $"""
Expr:
%s{Expr.format noAnnotation tc.Expr}

Expected: (no error)
Actual:   %s{format terr}
"""


type ExprTestCaseError =
    { Expr: Expr<unit>
      Expected: TypeError }

let exprTestCasesError: obj[] list =
    [ [| { Expr = ctor "UndefinedCtor" [] __LINE__
           Expected = NoSuchCtor(Ctor "UndefinedCtor") } |]
      [| { Expr = ctor "Foo" [ (litTrue __LINE__) ] __LINE__
           Expected = AssociatedValuesLenMismatch(Ctor "Foo", Set [ 1; 0 ]) } |]
      [| { Expr = ifExpr (litUnit __LINE__) (litUnit __LINE__) (litFalse __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = ifExpr (litTrue __LINE__) (litUnit __LINE__) (litFalse __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = matchExpr (litUnit __LINE__) [ (None, [], (litTrue __LINE__)) ] __LINE__
           Expected = NotUnion(tcUnit) } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (Some("Some"), [ "_" ], (litTrue __LINE__))
                   (Some("None"), [], (litUnit __LINE__)) ]
                 __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (Some("Some"), [ "_" ], (litTrue __LINE__))
                   (None, [ "_" ], (litUnit __LINE__)) ]
                 __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (Some("Some"), [ "x" ], varRef "x" __LINE__)
                   (None, [ "_" ], (litTrue __LINE__)) ]
                 __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = matchExpr (ctor "Foo" [] __LINE__) [] __LINE__
           Expected = EmptyMatch } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (None, [ "x"; "y" ], (litUnit __LINE__)) ]
                 __LINE__
           Expected = DefaultClauseArgumentsLenMustBe1([ Some(Var "x"); Some(Var "y") ]) } |]
      [| { Expr = varRef "undefined" __LINE__
           Expected = TypeEnvError(UnboundVariable(Var "undefined")) } |]
      [| { Expr = eq (tBool __LINE__) (litFalse __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = eq (tBool __LINE__) (litUnit __LINE__) (litFalse __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = eq (tBool __LINE__) (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = less (tNat __LINE__) (litFalse __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcNat ]) } |]
      [| { Expr = less (tNat __LINE__) (litNat 0u __LINE__) (litFalse __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcNat; tcBool ]) } |]
      [| { Expr = less (tBool __LINE__) (litTrue __LINE__) (litFalse __LINE__) __LINE__
           Expected = TypeNotDerived(tcBool, ClassOrd.name) } |]
      [| { Expr = boolNot (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = plus (tUnit __LINE__) (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassPlus.name) } |]
      [| { Expr = plus (tBool __LINE__) (litUnit __LINE__) (litTrue __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = plus (tBool __LINE__) (litTrue __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = minus (tUnit __LINE__) (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassMinus.name) } |]
      [| { Expr = minus (tNat __LINE__) (litUnit __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcNat ]) } |]
      [| { Expr = minus (tNat __LINE__) (litNat 0u __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcNat; tcUnit ]) } |]
      [| { Expr = times (tUnit __LINE__) (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassTimes.name) } |]
      [| { Expr = times (tBool __LINE__) (litUnit __LINE__) (litTrue __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = times (tBool __LINE__) (litTrue __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = size (tUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassSize.name) } |]
      [| { Expr = size (tSet (tUnit __LINE__) __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcSet tcUnit; tcUnit ]) } |]
      [| { Expr = size (tSet (tUnit __LINE__) __LINE__) (litEmpty (tSet (tBool __LINE__) __LINE__) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = filter (tUnit __LINE__) "x" (litTrue __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassEnum.name) } |]
      [| { Expr =
             filter
                 (tSet (tUnit __LINE__) __LINE__)
                 "x"
                 (litTrue __LINE__)
                 (litEmpty (tSet (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr =
             filter
                 (tSet (tUnit __LINE__) __LINE__)
                 "x"
                 (litUnit __LINE__)
                 (litEmpty (tSet (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = exists (tUnit __LINE__) "x" (litTrue __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassEnum.name) } |]
      [| { Expr =
             exists
                 (tSet (tUnit __LINE__) __LINE__)
                 "x"
                 (litTrue __LINE__)
                 (litEmpty (tSet (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr =
             exists
                 (tSet (tUnit __LINE__) __LINE__)
                 "x"
                 (litUnit __LINE__)
                 (litEmpty (tSet (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = tupleFst (litUnit __LINE__) __LINE__
           Expected = TupleIndexOutOfBounds(tcUnit, 0u) } |]
      [| { Expr = tupleFst (litTrue __LINE__) __LINE__
           Expected = NotTuple(tcBool) } |]
      [| { Expr = tupleSnd (litUnit __LINE__) __LINE__
           Expected = TupleIndexOutOfBounds(tcUnit, 1u) } |]
      [| { Expr = tupleSnd (litTrue __LINE__) __LINE__
           Expected = NotTuple(tcBool) } |]
      [| { Expr = listCons (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcList tcUnit ]) } |]
      [| { Expr = listCons (litUnit __LINE__) (litEmpty (tList (tBool __LINE__) __LINE__) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = listNth (litUnit __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcList (tcUncertain 0u) ]) } |]
      [| { Expr = listNth (litEmpty (tList (tBool __LINE__) __LINE__) __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcNat ]) } |]
      [| { Expr = setRange (litUnit __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcNat ]) } |]
      [| { Expr = setRange (litNat 0u __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcNat ]) } |]
      [| { Expr = setInsert (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcSet tcUnit ]) } |]
      [| { Expr = setInsert (litTrue __LINE__) (litEmpty (tSet (tUnit __LINE__) __LINE__) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = setMem (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcSet tcUnit ]) } |]
      [| { Expr = setMem (litTrue __LINE__) (litEmpty (tSet (tUnit __LINE__) __LINE__) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = mapAdd (litUnit __LINE__) (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcMap tcUnit tcUnit ]) } |]
      [| { Expr =
             mapAdd
                 (litUnit __LINE__)
                 (litTrue __LINE__)
                 (litEmpty (tMap (tBool __LINE__) (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr =
             mapAdd
                 (litTrue __LINE__)
                 (litUnit __LINE__)
                 (litEmpty (tMap (tBool __LINE__) (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = mapFindOpt (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcMap tcUnit (tcUncertain 0u) ]) } |]
      [| { Expr =
             mapFindOpt
                 (litUnit __LINE__)
                 (litEmpty (tMap (tBool __LINE__) (tBool __LINE__) __LINE__) __LINE__)
                 __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (Some("Some"), [ "x" ], boolNot (varRef "x" __LINE__) __LINE__)
                   (Some("None"), [], (litTrue __LINE__)) ]
                 __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |] ]

[<Theory>]
[<MemberData(nameof exprTestCasesError)>]
let inferExprError (tc: ExprTestCaseError) =
    let tOption =
        tUnion "option" [ ("Some", [ tVar 0u __LINE__ ]); ("None", []) ] __LINE__ in

    let tFoo = tUnion "foo" [ ("Foo", []) ] __LINE__ in
    let cm = CtorMap.from [ tOption; tFoo ] in
    let tenv = TypeCstrEnv.from [ ("GLOBAL", tcBool) ] in

    match postProcess (infer cm tenv tc.Expr init) with
    | Ok(actual, s) ->
        Assert.Fail
            $"""
Inferred as:
%s{Expr.format typeAnnotation actual}

TVar mapping:
%s{TypeCstrUncertainVar.format s.UncertainVarMap}
"""
    | Error terr ->
        Assert.True(
            tc.Expected = unwrapTypeError terr,
            $"""
Expected: %s{format tc.Expected}
Actual:   %s{format terr}"""
        )
