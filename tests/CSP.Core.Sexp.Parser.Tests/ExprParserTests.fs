module CSP.Core.Sexp.ExprParser.Tests

open Xunit
open FSharpPlus
open CSP.Core.Sexp
open CSP.Core.Sexp.ExprParser
open CSP.Core.Expr
open CSP.Core.ExprShorthand

type TestCase =
    { Input: string
      Expected: Expr<unit>
      Line: string }

let testCases: obj[] list =
    [ [| { Input = "unit"
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
      [| { Input = "(empty (nat list))"
           Expected = litNat 123u "1"
           Line = __LINE__ } |]
       ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    match parse (String.trimStartWhiteSpaces tc.Input) with
    | Error(err) -> Assert.Fail(SyntaxError.format err)
    | Ok(actual) ->
        Assert.True(
            tc.Expected = actual,
            $"""line %s{tc.Line}

Expected:
%s{format noAnnotation tc.Expected}

Actual:
%s{format noAnnotation actual}
"""
        )
