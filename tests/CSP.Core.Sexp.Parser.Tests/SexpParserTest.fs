module CSP.Core.Sexp.SexpParser

open Xunit
open CSP.Core.LineNum
open CSP.Core.Sexp
open CSP.Core.Sexp.SyntaxError
open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.SexpParser

type TestCaseOk =
    { Input: string
      Expected: Sexp
      Line: LineNum }

let testCasesOk: obj[] list =
    [ [| { Input = "0"
           Expected = Atom("0", "1")
           Line = __LINE__ } |]
      [| { Input = "()"
           Expected = Sexps([], "1")
           Line = __LINE__ } |]
      [| { Input = "(0)"
           Expected = Sexps([ Atom("0", "1") ], "1")
           Line = __LINE__ } |]
      [| { Input = "(0 1 2)"
           Expected = Sexps([ Atom("0", "1"); Atom("1", "1"); Atom("2", "1") ], "1")
           Line = __LINE__ } |]
      [| { Input = "((0) 1 2)"
           Expected = Sexps([ Sexps([ Atom("0", "1") ], "1"); Atom("1", "1"); Atom("2", "1") ], "1")
           Line = __LINE__ } |]
      [| { Input = "(0 (1) 2)"
           Expected = Sexps([ Atom("0", "1"); Sexps([ Atom("1", "1") ], "1"); Atom("2", "1") ], "1")
           Line = __LINE__ } |]
      [| { Input = "(0 1 (2))"
           Expected = Sexps([ Atom("0", "1"); Atom("1", "1"); Sexps([ Atom("2", "1") ], "1") ], "1")
           Line = __LINE__ } |]
      [| { Input =
             """(
0
1
2
)
"""
           Expected = Sexps([ Atom("0", "2"); Atom("1", "3"); Atom("2", "4") ], "5")
           Line = __LINE__ } |]
      [| { Input =
             """
; comment
(0 1 2)
"""
           Expected = Sexps([ Atom("0", "3"); Atom("1", "3"); Atom("2", "3") ], "3")
           Line = __LINE__ } |]
      [| { Input =
             """
(0 1 2) ; comment
"""
           Expected = Sexps([ Atom("0", "2"); Atom("1", "2"); Atom("2", "2") ], "2")
           Line = __LINE__ } |]
      [| { Input =
             """
(0 1 2) ; ()comment
"""
           Expected = Sexps([ Atom("0", "2"); Atom("1", "2"); Atom("2", "2") ], "2")
           Line = __LINE__ } |]
      [| { Input =
             """
(0
    ; comment
    1
    2)
"""
           Expected = Sexps([ Atom("0", "2"); Atom("1", "4"); Atom("2", "5") ], "5")
           Line = __LINE__ } |]
      
       ]

[<Theory>]
[<MemberData(nameof testCasesOk)>]
let ok (tc: TestCaseOk) =
    match parse tc.Input with
    | Error(err) ->
        Assert.Fail(
            $"""line %s{tc.Line}

Error: %s{SyntaxError.format err}
"""
        )
    | Ok(actual) ->
        Assert.True(
            tc.Expected = actual,
            $"""line %s{tc.Line}

Expected: %A{tc.Expected}
Actual:   %A{actual}
"""
        )

type TestCaseNg =
    { Input: string
      Expected: SyntaxError
      Line: LineNum }

let testCasesNg: obj[] list =
    [ [| { Input = ""
           Expected = EmptyAtom
           Line = __LINE__ } |]
      [| { Input = "("
           Expected = ParensNotClosed
           Line = __LINE__ } |]
      [| { Input = ")"
           Expected = EmptyAtom
           Line = __LINE__ } |]
      [| { Input = "(1 (0)"
           Expected = ParensNotClosed
           Line = __LINE__ } |]
      [| { Input = "())"
           Expected = GarbageInTail(")")
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCasesNg)>]
let ng (tc: TestCaseNg) =
    match parse tc.Input with
    | Error(actual) ->
        Assert.True(
            tc.Expected = actual,
            $"""line %s{tc.Line}

Expected: %s{SyntaxError.format tc.Expected}
Actual:   %s{SyntaxError.format actual}
"""
        )
    | Ok(parsed) ->
        Assert.Fail(
            $"""line %s{tc.Line}

Parsed as: %s{format parsed}
"""
        )
