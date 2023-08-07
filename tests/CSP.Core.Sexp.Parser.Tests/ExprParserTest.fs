module CSP.Core.Sexp.ExprParser

open Xunit
open CSP.Core.Sexp
open CSP.Core.Util
open CSP.Core.Sexp.ExprParser
open CSP.Core.Expr
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand

type TestCase =
    { Input: string
      Expected: Expr<unit>
      Line: string }

let testCases: obj[] list =
    [ [| { Input = "()"
           Expected = litUnit "1"
           Line = __LINE__ } |]
      [| { Input = "true"
           Expected = litTrue "1"
           Line = __LINE__ } |]
      [| { Input = "false"
           Expected = litFalse "1"
           Line = __LINE__ } |]
      [| { Input = "0"
           Expected = litNat 0u "1"
           Line = __LINE__ } |]
      [| { Input = "123"
           Expected = litNat 123u "1"
           Line = __LINE__ } |]
      [| { Input = "x"
           Expected = varRef "x" "1"
           Line = __LINE__ } |]
      [| { Input = "acc"
           Expected = varRef "acc" "1"
           Line = __LINE__ } |]
      [| { Input = "x1"
           Expected = varRef "x1" "1"
           Line = __LINE__ } |]
      [| { Input = "1x"
           Expected = varRef "1x" "1"
           Line = __LINE__ } |]
      [| { Input = "None"
           Expected = ctor "None" [] "1"
           Line = __LINE__ } |]
      [| { Input = "(empty (list nat))"
           Expected = litEmpty (tList tNat) "1"
           Line = __LINE__ } |]
      [| { Input = "(None)"
           Expected = ctor "None" [] "1"
           Line = __LINE__ } |]
      [| { Input = "(Some true)"
           Expected = ctor "Some" [ litTrue "1" ] "1"
           Line = __LINE__ } |]
      [| { Input = "(Tuple true false)"
           Expected = ctor "Tuple" [ litTrue "1"; litFalse "1" ] "1"
           Line = __LINE__ } |]
      [| { Input = "(tuple true false)"
           Expected = tuple2 (litTrue "1") (litFalse "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(tuple 0 1 2)"
           Expected = tuple2 (litNat 0u "1") (tuple2 (litNat 1u "1") (litNat 2u "1") "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(if true 0 1)"
           Expected = ifExpr (litTrue "1") (litNat 0u "1") (litNat 1u "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(match None (None true) (Some x false))"
           Expected =
             matchExpr (ctor "None" [] "1") [ (("None", []), litTrue "1"); (("Some", [ "x" ]), litFalse "1") ] "1"
           Line = __LINE__ } |]
      [| { Input = "(match None (None true) (Some _ false))"
           Expected =
             matchExpr (ctor "None" [] "1") [ (("None", []), litTrue "1"); (("Some", [ "_" ]), litFalse "1") ] "1"
           Line = __LINE__ } |]
      [| { Input = "(match None (None true) (_ _ false))"
           Expected = matchExpr (ctor "None" [] "1") [ (("None", []), litTrue "1"); (("_", [ "_" ]), litFalse "1") ] "1"
           Line = __LINE__ } |]
      [| { Input = "(eq nat 0 1)"
           Expected = eq tNat (litNat 0u "1") (litNat 1u "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(less nat 0 1)"
           Expected = less tNat (litNat 0u "1") (litNat 1u "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(plus nat 0 1)"
           Expected = plus tNat (litNat 0u "1") (litNat 1u "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(minus nat 0 1)"
           Expected = minus tNat (litNat 0u "1") (litNat 1u "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(times nat 0 1)"
           Expected = times tNat (litNat 0u "1") (litNat 1u "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(size (list nat) x)"
           Expected = size (tList tNat) (varRef "x" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(filter (list nat) x true y)"
           Expected = filter (tList tNat) "x" (litTrue "1") (varRef "y" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(exists (list nat) x true y)"
           Expected = exists (tList tNat) "x" (litTrue "1") (varRef "y" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(contains (list nat) 0 xs)"
           Expected = contains (tList tNat) (litNat 0u "1") (varRef "xs" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(not x)"
           Expected = boolNot (varRef "x" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(fst x)"
           Expected = tupleFst (varRef "x" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(snd x)"
           Expected = tupleSnd (varRef "x" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(cons 0 xs)"
           Expected = listCons (litNat 0u "1") (varRef "xs" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(nth xs 0)"
           Expected = listNth (varRef "xs" "1") (litNat 0u "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(range 0 1)"
           Expected = setRange (litNat 0u "1") (litNat 1u "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(insert x s)"
           Expected = setInsert (varRef "x" "1") (varRef "s" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(remove x s)"
           Expected = setRemove (varRef "x" "1") (varRef "s" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(add k v m)"
           Expected = mapAdd (varRef "k" "1") (varRef "v" "1") (varRef "m" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(findOpt k m)"
           Expected = mapFindOpt (varRef "k" "1") (varRef "m" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(univ nat)"
           Expected = univ tNat "1"
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    let sexp = SexpParser.parse tc.Input |> ResultEx.get SyntaxError.format

    match parse sexp with
    | Error(err) ->
        Assert.Fail(
            $"""line %s{tc.Line}

%s{ExprSyntaxError.format err}
"""
        )
    | Ok(actual) ->
        Assert.True(
            tc.Expected = actual,
            $"""line %s{tc.Line}

Expected:
%s{format noAnnotation tc.Expected}

%A{tc.Expected}

Actual:
%s{format noAnnotation actual}

%A{actual}
"""
        )
