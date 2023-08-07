module CSP.Core.Sexp.TypeParser.Tests

open Xunit
open FSharpPlus
open CSP.Core.Type
open CSP.Core.TypeShorthand
open CSP.Core.Sexp
open CSP.Core.Sexp.TypeParser

type TestCase =
    { Input: string
      Expected: Type
      Line: string }

let testCases: obj[] list =
    [ [| { Input = "unit"
           Expected = tUnit
           Line = __LINE__ } |]
      [| { Input = "'a"
           Expected = tVar 0u
           Line = __LINE__ } |]
      [| { Input = "'b123"
           Expected = tVar 123u
           Line = __LINE__ } |]
      [| { Input = "nat"
           Expected = tNat
           Line = __LINE__ } |]
      [| { Input = "bool"
           Expected = tBool
           Line = __LINE__ } |]
      [| { Input = "tuple (nat bool)"
           Expected = tTuple2 tNat tBool
           Line = __LINE__ } |]
      [| { Input = "tuple (nat (tuple bool unit))"
           Expected = tTuple2 tNat (tTuple2 tBool tUnit)
           Line = __LINE__ } |]
      [| { Input = "set (nat)"
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Input = "set (set nat)"
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Input = "list (nat)"
           Expected = tList tNat
           Line = __LINE__ } |]
      [| { Input = "list (nat list)"
           Expected = tList tNat
           Line = __LINE__ } |]
      [| { Input = "map (nat bool)"
           Expected = tMap tNat tBool
           Line = __LINE__ } |]
      [| { Input = "map (nat (list nat))"
           Expected = tMap tNat tBool
           Line = __LINE__ } |]
      [| { Input = "map ((list nat) nat)"
           Expected = tMap tNat tBool
           Line = __LINE__ } |]
      [| { Input = "foo ()"
           Expected = tUnion "foo" []
           Line = __LINE__ } |]
      [| { Input = "option (unit)"
           Expected = tUnion "option" [tUnit]
           Line = __LINE__ } |]
      [| { Input = "either (unit unit)"
           Expected = tUnion "either" [tUnit; tUnit]
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
%s{format tc.Expected}

Actual:
%s{format actual}
"""
        )
