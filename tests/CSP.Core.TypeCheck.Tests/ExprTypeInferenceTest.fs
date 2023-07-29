module CSP.Core.ExprTypeInferenceTest

open CSP.Core.Util
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
           Expected = tUnit } |]
      [| { Expr = litTrue __LINE__
           Expected = tBool } |]
      [| { Expr = litFalse __LINE__
           Expected = tBool } |]
      [| { Expr = litNat 0u __LINE__
           Expected = tNat } |]
      [| { Expr = litEmpty (tSet tUnit) __LINE__
           Expected = tSet tUnit } |]
      [| { Expr = litEmpty (tList tUnit) __LINE__
           Expected = tList tUnit } |]
      [| { Expr = litEmpty (tMap tBool tUnit) __LINE__
           Expected = tMap tBool tUnit } |]
      [| { Expr = ctor "Foo" [] __LINE__
           Expected = tUnion "foo" [ ("Foo", []) ] } |]
      [| { Expr = ifExpr (litTrue __LINE__) (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = tUnit } |]
      [| { Expr = ifExpr (litTrue __LINE__) (litTrue __LINE__) (litFalse __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litUnit __LINE__ ] __LINE__)
                 [ (Some("Some"), [ "x" ], litTrue __LINE__)
                   (Some("None"), [], (litFalse __LINE__)) ]
                 __LINE__
           Expected = tBool } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litUnit __LINE__ ] __LINE__)
                 [ (Some("Some"), [ "x" ], ctor "Some" [ varRef "x" __LINE__ ] __LINE__)
                   (None, [ "x" ], varRef "x" __LINE__) ]
                 __LINE__
           Expected = tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ] } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litUnit __LINE__ ] __LINE__)
                 [ (Some "Some", [ "x" ], ctor "Some" [ varRef "x" __LINE__ ] __LINE__)
                   (None, [ "x" ], varRef "x" __LINE__) ]
                 __LINE__
           Expected = tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ] } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litUnit __LINE__ ] __LINE__)
                 [ (Some "Some", [ "_" ], litFalse __LINE__); (None, [ "_" ], litTrue __LINE__) ]
                 __LINE__
           Expected = tBool } |]
      [| { Expr = varRef "GLOBAL" __LINE__
           Expected = tBool } |]
      [| { Expr = eq (tSet tNat) (litEmpty (tSet tNat) __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr = boolNot (litTrue __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr = less (tSet tNat) (litEmpty (tSet tNat) __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr = plus tUnit (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = tUnit } |]
      [| { Expr = plus (tSet tNat) (litEmpty (tSet tNat) __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__
           Expected = tSet tNat } |]
      [| { Expr = minus (tSet tNat) (litEmpty (tSet tNat) __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__
           Expected = tSet tNat } |]
      [| { Expr = minus tUnit (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = tUnit } |]
      [| { Expr = times (tSet tNat) (litEmpty (tSet tNat) __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__
           Expected = tSet tNat } |]
      [| { Expr = size (tList tBool) (listCons (litTrue __LINE__) (litEmpty (tList tBool) __LINE__) __LINE__) __LINE__
           Expected = tNat } |]
      [| { Expr = size (tSet tNat) (litEmpty (tSet tNat) __LINE__) __LINE__
           Expected = tNat } |]
      [| { Expr = size (tMap tNat tNat) (litEmpty (tMap tNat tNat) __LINE__) __LINE__
           Expected = tNat } |]
      [| { Expr = filter (tList tUnit) "x" (litTrue __LINE__) (litEmpty (tList tUnit) __LINE__) __LINE__
           Expected = tList tUnit } |]
      [| { Expr = filter (tSet tUnit) "x" (litTrue __LINE__) (litEmpty (tSet tUnit) __LINE__) __LINE__
           Expected = tSet tUnit } |]
      [| { Expr = filter (tList tBool) "x" (varRef "x" __LINE__) (litEmpty (tList tBool) __LINE__) __LINE__
           Expected = tList tBool } |]
      [| { Expr = exists (tList tUnit) "x" (litTrue __LINE__) (litEmpty (tList tUnit) __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr = exists (tList tBool) "x" (varRef "x" __LINE__) (litEmpty (tList tBool) __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr = tuple2 (litNat 0u __LINE__) (litTrue __LINE__) __LINE__
           Expected = tTuple2 tNat tBool } |]
      [| { Expr = tupleFst (tuple2 (litTrue __LINE__) (litNat 0u __LINE__) __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr = tupleSnd (tuple2 (litTrue __LINE__) (litNat 0u __LINE__) __LINE__) __LINE__
           Expected = tNat } |]
      [| { Expr = listCons (litTrue __LINE__) (litEmpty (tList tBool) __LINE__) __LINE__
           Expected = tList tBool } |]
      [| { Expr = listNth (litEmpty (tList tBool) __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr = setRange (litNat 0u __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = tSet tNat } |]
      [| { Expr = setInsert (litNat 0u __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__
           Expected = tSet tNat } |]
      [| { Expr = setMem (litNat 0u __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__
           Expected = tBool } |]
      [| { Expr = mapAdd (litNat 0u __LINE__) (litUnit __LINE__) (litEmpty (tMap tNat tUnit) __LINE__) __LINE__
           Expected = tMap tNat tUnit } |]
      [| { Expr = mapFindOpt (litNat 0u __LINE__) (litEmpty (tMap tNat tUnit) __LINE__) __LINE__
           Expected = tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ] } |]
      [| { Expr = univ (tMap tNat tUnit) __LINE__
           Expected = tSet (tMap tNat tUnit) } |]
      [| { Expr =
             tuple2 (ctor "Some" [ litUnit __LINE__ ] __LINE__) (ctor "Some" [ litTrue __LINE__ ] __LINE__) __LINE__
           Expected =
             tTuple2
                 (tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ])
                 (tUnion "option" [ ("Some", [ tBool ]); ("None", []) ]) } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ litTrue __LINE__ ] __LINE__)
                 [ (Some("Some"), [ "x" ], boolNot (varRef "x" __LINE__) __LINE__)
                   (Some("None"), [], litTrue __LINE__) ]
                 __LINE__
           Expected = tBool } |] ]

[<Theory>]
[<MemberData(nameof exprTestCasesOk)>]
let inferExprOk (tc: ExprTestCaseOk) =
    let tOption = tUnion "option" [ ("Some", [ tVar 0u ]); ("None", []) ] in

    let tFoo = tUnion "foo" [ ("Foo", []) ] in
    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tOption; tFoo ]) in
    let tenv = TypeCstrEnv.from [ ("GLOBAL", tcBool) ] in

    match postProcess (infer cm tenv tc.Expr init) with
    | Ok(actual, s) ->
        Assert.True(
            tc.Expected = get actual,
            $"""line %s{line actual}

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
           Expected = NoCtors } |]
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
           Expected = NoCtors } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (Some "None", [], litUnit __LINE__); (None, [ "x"; "y" ], litUnit __LINE__) ]
                 __LINE__
           Expected = DefaultClauseArgumentsLenMustBe1([ Some(Var "x"); Some(Var "y") ]) } |]
      [| { Expr = varRef "undefined" __LINE__
           Expected = TypeEnvError(UnboundVariable(Var "undefined")) } |]
      [| { Expr = eq tBool (litFalse __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = eq tBool (litUnit __LINE__) (litFalse __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = eq tBool (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = less tNat (litFalse __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcNat ]) } |]
      [| { Expr = less tNat (litNat 0u __LINE__) (litFalse __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcNat; tcBool ]) } |]
      [| { Expr = less tBool (litTrue __LINE__) (litFalse __LINE__) __LINE__
           Expected = TypeNotDerived(tcBool, ClassOrd.name) } |]
      [| { Expr = boolNot (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = plus tBool (litUnit __LINE__) (litTrue __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = plus tBool (litTrue __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = minus tNat (litUnit __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcNat ]) } |]
      [| { Expr = minus tNat (litNat 0u __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcNat; tcUnit ]) } |]
      [| { Expr = times tUnit (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassTimes.name) } |]
      [| { Expr = times tBool (litUnit __LINE__) (litTrue __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = times tBool (litTrue __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = size tUnit (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassSize.name) } |]
      [| { Expr = size (tSet tUnit) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcSet tcUnit; tcUnit ]) } |]
      [| { Expr = size (tSet tUnit) (litEmpty (tSet tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = filter tUnit "x" (litTrue __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassEnum.name) } |]
      [| { Expr = filter (tSet tUnit) "x" (litTrue __LINE__) (litEmpty (tSet tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = filter (tSet tUnit) "x" (litUnit __LINE__) (litEmpty (tSet tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = exists tUnit "x" (litTrue __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeNotDerived(tcUnit, ClassEnum.name) } |]
      [| { Expr = exists (tSet tUnit) "x" (litTrue __LINE__) (litEmpty (tSet tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = exists (tSet tUnit) "x" (litUnit __LINE__) (litEmpty (tSet tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = tupleFst (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcTuple2 (tcUncertain 0u) (tcUncertain 1u) ]) } |]
      [| { Expr = tupleSnd (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcTuple2 (tcUncertain 0u) (tcUncertain 1u) ]) } |]
      [| { Expr = listCons (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcList tcUnit ]) } |]
      [| { Expr = listCons (litUnit __LINE__) (litEmpty (tList tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = listNth (litUnit __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcList (tcUncertain 0u) ]) } |]
      [| { Expr = listNth (litEmpty (tList tBool) __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcNat ]) } |]
      [| { Expr = setRange (litUnit __LINE__) (litNat 0u __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcNat ]) } |]
      [| { Expr = setRange (litNat 0u __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcNat ]) } |]
      [| { Expr = setInsert (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcSet tcUnit ]) } |]
      [| { Expr = setInsert (litTrue __LINE__) (litEmpty (tSet tUnit) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = setMem (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcSet tcUnit ]) } |]
      [| { Expr = setMem (litTrue __LINE__) (litEmpty (tSet tUnit) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr = mapAdd (litUnit __LINE__) (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcMap tcUnit tcUnit ]) } |]
      [| { Expr = mapAdd (litUnit __LINE__) (litTrue __LINE__) (litEmpty (tMap tBool tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = mapAdd (litTrue __LINE__) (litUnit __LINE__) (litEmpty (tMap tBool tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr = mapFindOpt (litUnit __LINE__) (litUnit __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcMap tcUnit (tcUncertain 0u) ]) } |]
      [| { Expr = mapFindOpt (litUnit __LINE__) (litEmpty (tMap tBool tBool) __LINE__) __LINE__
           Expected = TypeMismatch(Set [ tcBool; tcUnit ]) } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (Some("Some"), [ "x" ], boolNot (varRef "x" __LINE__) __LINE__)
                   (Some("None"), [], (litTrue __LINE__)) ]
                 __LINE__
           Expected = TypeMismatch(Set [ tcUnit; tcBool ]) } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (Some("Some"), [ "x" ], litFalse __LINE__) ]
                 __LINE__
           Expected = NotExhausted(Set[Ctor "None"]) } |]
      [| { Expr =
             matchExpr
                 (ctor "Some" [ (litUnit __LINE__) ] __LINE__)
                 [ (Some("Some"), [ "x" ], litFalse __LINE__)
                   (Some("None"), [], litFalse __LINE__)
                   (Some("Foo"), [], litFalse __LINE__) ]
                 __LINE__
           Expected = UnionNameMismatch("option", "foo") } |] ]

[<Theory>]
[<MemberData(nameof exprTestCasesError)>]
let inferExprError (tc: ExprTestCaseError) =
    let tOption = tUnion "option" [ ("Some", [ tVar 0u ]); ("None", []) ] in

    let tFoo = tUnion "foo" [ ("Foo", []) ] in
    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tOption; tFoo ]) in
    let tenv = TypeCstrEnv.from [ ("GLOBAL", tcBool) ] in

    match postProcess (infer cm tenv tc.Expr init) with
    | Ok(actual, s) ->
        Assert.Fail
            $"""line %s{line actual}

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
