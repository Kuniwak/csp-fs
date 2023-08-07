module CSP.Core.Sexp.StmtParser

open CSP.Core.Util
open Xunit
open CSP.Core.LineNum
open CSP.Core.ProcShorthand
open CSP.Core.TypeShorthand
open CSP.Core.ExprShorthand
open CSP.Core.Sexp
open CSP.Core.Sexp.StmtParser
open CSP.Core.Sexp.Stmt

type TestCase =
    { Input: string
      Expected: Stmt
      Line: LineNum }

let testCases: obj[] list =
    [ [| { Input = "(def P () stop)"
           Expected = ProcDecl(("P", []), stop "1")
           Line = __LINE__ } |]
      [| { Input = "(def P ((x nat)) stop)"
           Expected = ProcDecl(("P", [ ("x", tNat) ]), stop "1")
           Line = __LINE__ } |]
      [| { Input = "(def P ((x nat) (y nat)) stop)"
           Expected = ProcDecl(("P", [ ("x", tNat); ("y", tNat) ]), stop "1")
           Line = __LINE__ } |]
      [| { Input = "(init P ())"
           Expected = Init("P", [])
           Line = __LINE__ } |]
      [| { Input = "(init P (true))"
           Expected = Init("P", [ litTrue "1" ])
           Line = __LINE__ } |]
      [| { Input = "(init P (true false))"
           Expected = Init("P", [ litTrue "1"; litFalse "1" ])
           Line = __LINE__ } |]
      [| { Input = "(type () event A)"
           Expected = UnionDecl(([], "event"), [ ("A", []) ])
           Line = __LINE__ } |]
      [| { Input = "(type () event (A))"
           Expected = UnionDecl(([], "event"), [ ("A", []) ])
           Line = __LINE__ } |]
      [| { Input = "(type () event (A nat))"
           Expected = UnionDecl(([], "event"), [ ("A", [ tNat ]) ])
           Line = __LINE__ } |]
      [| { Input = "(type () event (A nat bool))"
           Expected = UnionDecl(([], "event"), [ ("A", [ tNat; tBool ]) ])
           Line = __LINE__ } |]
      [| { Input = "(type ('0) event (A '0))"
           Expected = UnionDecl(([ 0u ], "event"), [ ("A", [ tVar 0u ]) ])
           Line = __LINE__ } |]
      [| { Input = "(type ('0 '1) event (A '0 '1))"
           Expected = UnionDecl(([ 0u; 1u ], "event"), [ ("A", [ tVar 0u; tVar 1u ]) ])
           Line = __LINE__ } |]
      [| { Input = "(type () event A B)"
           Expected = UnionDecl(([], "event"), [ ("A", []); ("B", []) ])
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    let sexp = SexpParser.parse tc.Input |> ResultEx.get SyntaxError.format in

    match parse sexp with
    | Error(err) ->
        Assert.Fail(
            $"""line %s{tc.Line}

%s{StmtSyntaxError.format err}
"""
        )
    | Ok(actual) ->
        Assert.True(
            tc.Expected = actual,
            $"""line %s{tc.Line}

Expected:
%s{format tc.Expected}

%A{tc.Expected}

Actual:
%s{format actual}

%A{actual}
"""
        )
