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
    [ [| { Input = "(proc P () stop)"
           Expected = ProcDecl(("P", []), stop "1")
           Line = __LINE__ } |]
      [| { Input = "(proc P ((x nat)) stop)"
           Expected = ProcDecl(("P", [ ("x", tNat) ]), stop "1")
           Line = __LINE__ } |]
      [| { Input = "(proc P ((x nat) (y nat)) stop)"
           Expected = ProcDecl(("P", [ ("x", tNat); ("y", tNat) ]), stop "1")
           Line = __LINE__ } |]
      [| { Input = "(type event A)"
           Expected = UnionDecl(([], "event"), [ ("A", []) ])
           Line = __LINE__ } |]
      [| { Input = "(type event (A))"
           Expected = UnionDecl(([], "event"), [ ("A", []) ])
           Line = __LINE__ } |]
      [| { Input = "(type event (A nat))"
           Expected = UnionDecl(([], "event"), [ ("A", [ tNat ]) ])
           Line = __LINE__ } |]
      [| { Input = "(type event (A nat bool))"
           Expected = UnionDecl(([], "event"), [ ("A", [ tNat; tBool ]) ])
           Line = __LINE__ } |]
      [| { Input = "(type () event A)"
           Expected = UnionDecl(([], "event"), [ ("A", []) ])
           Line = __LINE__ } |]
      [| { Input = "(type ('0) event (A '0))"
           Expected = UnionDecl(([ 0u ], "event"), [ ("A", [ tVar 0u ]) ])
           Line = __LINE__ } |]
      [| { Input = "(type ('0 '1) event (A '0 '1))"
           Expected = UnionDecl(([ 0u; 1u ], "event"), [ ("A", [ tVar 0u; tVar 1u ]) ])
           Line = __LINE__ } |]
      [| { Input = "(type event A B)"
           Expected = UnionDecl(([], "event"), [ ("A", []); ("B", []) ])
           Line = __LINE__ } |]
      [| { Input = "(type ('0 '1) event (A '0) (B '1))"
           Expected = UnionDecl(([0u; 1u], "event"), [ ("A", [ tVar 0u ]); ("B", [ tVar 1u ]) ])
           Line = __LINE__ } |]
      [| { Input = "(const x 0)"
           Expected = GlobalVarDecl("x", litNat 0u "1")
           Line = __LINE__ } |]
       ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    let sexp = SexpParser.parse tc.Input |> ResultEx.getValue SyntaxError.format in

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
