module CSP.Core.Sexp.TypeParser.Tests

open Xunit
open FSharpPlus
open CSP.Core.Sexp
open CSP.Core.Sexp.TypeParser
open CSP.Core.Type
open CSP.Core.TypeShorthand

type TestCase =
    { Input: string
      Expected: Type
      Line: string }

let testCases: obj[] list =
    [ [| { Input = "(unit)"
           Expected = tUnit
           Line = __LINE__ } |]
      [| { Input = "('0)"
           Expected = tVar 0u
           Line = __LINE__ } |]
      [| { Input = "('123)"
           Expected = tVar 123u
           Line = __LINE__ } |]
      [| { Input = "(nat)"
           Expected = tNat
           Line = __LINE__ } |]
      [| { Input = "(bool)"
           Expected = tBool
           Line = __LINE__ } |]
      [| { Input = "(tuple nat bool)"
           Expected = tTuple2 tNat tBool
           Line = __LINE__ } |]
      [| { Input = "(tuple nat (tuple bool unit))"
           Expected = tTuple2 tNat (tTuple2 tBool tUnit)
           Line = __LINE__ } |]
      [| { Input = "(set nat)"
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Input = "(list nat)"
           Expected = tList tNat
           Line = __LINE__ } |]
      [| { Input = "(map nat bool)"
           Expected = tMap tNat tBool
           Line = __LINE__ } |]
      [| { Input = "(union option (Some '0) (None))"
           Expected = tMap tNat tBool
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
