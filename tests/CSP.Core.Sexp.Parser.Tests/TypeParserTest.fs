module CSP.Core.Sexp.TypeParser

open Xunit
open CSP.Core.Util
open CSP.Core
open CSP.Core.Type
open CSP.Core.TypeShorthand
open CSP.Core.Sexp
open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.TypeSyntaxError
open CSP.Core.Sexp.TypeParser

type TestCaseOk =
    { Input: string
      Expected: Type
      Line: string }

let testCasesOk: obj[] list =
    [ [| { Input = "unit"
           Expected = tUnit
           Line = __LINE__ } |]
      [| { Input = "'0"
           Expected = tVar 0u
           Line = __LINE__ } |]
      [| { Input = "'123"
           Expected = tVar 123u
           Line = __LINE__ } |]
      [| { Input = "nat"
           Expected = tNat
           Line = __LINE__ } |]
      [| { Input = "bool"
           Expected = tBool
           Line = __LINE__ } |]
      [| { Input = "(tuple nat bool)"
           Expected = tTuple2 tNat tBool
           Line = __LINE__ } |]
      [| { Input = "(tuple nat bool unit)"
           Expected = tTuple2 tNat (tTuple2 tBool tUnit)
           Line = __LINE__ } |]
      [| { Input = "(set nat)"
           Expected = tSet tNat
           Line = __LINE__ } |]
      [| { Input = "(set (set nat))"
           Expected = tSet (tSet tNat)
           Line = __LINE__ } |]
      [| { Input = "(list nat)"
           Expected = tList tNat
           Line = __LINE__ } |]
      [| { Input = "(list (list nat))"
           Expected = tList (tList tNat)
           Line = __LINE__ } |]
      [| { Input = "(map nat bool)"
           Expected = tMap tNat tBool
           Line = __LINE__ } |]
      [| { Input = "(map nat (list bool))"
           Expected = tMap tNat (tList tBool)
           Line = __LINE__ } |]
      [| { Input = "(map (list nat) bool)"
           Expected = tMap (tList tNat) tBool
           Line = __LINE__ } |]
      [| { Input = "foo"
           Expected = tUnion "foo" []
           Line = __LINE__ } |]
      [| { Input = "(option unit)"
           Expected = tUnion "option" [ tUnit ]
           Line = __LINE__ } |]
      [| { Input = "(either unit unit)"
           Expected = tUnion "either" [ tUnit; tUnit ]
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCasesOk)>]
let ok (tc: TestCaseOk) =
    let sexp = SexpParser.parse tc.Input |> ResultEx.get SyntaxError.format

    match parse sexp with
    | Error(err) ->
        Assert.Fail(
            $"""line %s{tc.Line}

Error: %s{format err}
"""
        )
    | Ok(actual) ->
        Assert.True(
            tc.Expected = actual,
            $"""line %s{tc.Line}

Expected:
%s{Type.format tc.Expected}

Actual:
%s{Type.format actual}
"""
        )

type TestCaseNg =
    { Input: string
      Expected: TypeSyntaxError
      Line: string }

let testCasesNg: obj[] list =
    [ [| { Input = "()"
           Expected = MapperError(MapperError.UnexpectedEmpty)
           Line = __LINE__ } |]
      [| { Input = "(())"
           Expected = MapperError(MapperError.UnexpectedList([]))
           Line = __LINE__ } |]
      [| { Input = "'a"
           Expected = InvalidTVarId("'a")
           Line = __LINE__ } |]
      [| { Input = "(tuple)"
           Expected = MapperError(MapperError.TooFewArguments([]))
           Line = __LINE__ } |]
      [| { Input = "(tuple nat)"
           Expected = MapperError(MapperError.TooFewArguments([ Atom("nat", "1") ]))
           Line = __LINE__ } |]
      [| { Input = "(set)"
           Expected = MapperError(MapperError.TooFewArguments([]))
           Line = __LINE__ } |]
      [| { Input = "(set nat bool)"
           Expected = MapperError(MapperError.TooMuchArguments([ Atom("nat", "1"); Atom("bool", "1") ]))
           Line = __LINE__ } |]
      [| { Input = "(list)"
           Expected = MapperError(MapperError.TooFewArguments([]))
           Line = __LINE__ } |]
      [| { Input = "(list nat bool)"
           Expected = MapperError(MapperError.TooMuchArguments([ Atom("nat", "1"); Atom("bool", "1") ]))
           Line = __LINE__ } |]
      [| { Input = "(map nat)"
           Expected = MapperError(MapperError.TooFewArguments([ Atom("nat", "1") ]))
           Line = __LINE__ } |]
      [| { Input = "(map nat bool unit)"
           Expected =
             MapperError(MapperError.TooMuchArguments([ Atom("nat", "1"); Atom("bool", "1"); Atom("unit", "1") ]))
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCasesNg)>]
let ng (tc: TestCaseNg) =
    let sexp = SexpParser.parse tc.Input |> ResultEx.get SyntaxError.format

    match parse sexp with
    | Ok(t) ->
        Assert.Fail(
            $"""line %s{tc.Line}

Parsed as: %s{Type.format t}
"""
        )
    | Error(actual) ->
        Assert.True(
            tc.Expected = actual,
            $"""line %s{tc.Line}

Expected:
%s{format tc.Expected}

Actual:
%s{format actual}
"""
        )
