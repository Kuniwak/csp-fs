module CSP.Core.Sexp.ProcParser

open Xunit
open CSP.Core.Sexp
open CSP.Core.Util
open CSP.Core.Sexp.ProcParser
open CSP.Core.Expr
open CSP.Core.ExprShorthand
open CSP.Core.Proc
open CSP.Core.ProcShorthand

type TestCase =
    { Input: string
      Expected: Proc<unit>
      Line: string }

let testCases: obj[] list =
    [ [| { Input = "(unwind P)"
           Expected = unwind "P" [] "1"
           Line = __LINE__ } |]
      [| { Input = "(unwind P true)"
           Expected = unwind "P" [ litTrue "1" ] "1"
           Line = __LINE__ } |]
      [| { Input = "(unwind P true false)"
           Expected = unwind "P" [ litTrue "1"; litFalse "1" ] "1"
           Line = __LINE__ } |]
      [| { Input = "(unwind P true false)"
           Expected = unwind "P" [ litTrue "1"; litFalse "1" ] "1"
           Line = __LINE__ } |]
      [| { Input = "stop"
           Expected = stop "1"
           Line = __LINE__ } |]
      [| { Input = "skip"
           Expected = skip "1"
           Line = __LINE__ } |]
      [| { Input = "(prefix true stop)"
           Expected = prefix (litTrue "1") (stop "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(prefixRecv y x stop)"
           Expected = prefixRecv (varRef "y" "1") "x" (stop "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(in stop stop)"
           Expected = intCh (stop "1") (stop "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(ex stop stop)"
           Expected = extCh (stop "1") (stop "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(seq stop stop)"
           Expected = seq (stop "1") (stop "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(if x stop stop)"
           Expected = ``if`` (varRef "x" "1") (stop "1") (stop "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(match x (Ch x stop))"
           Expected = ``match`` (varRef "x" "1") [ (("Ch", ["x"]), stop "1") ] "1"
           Line = __LINE__ } |]
      [| { Input = "(match x (None stop) (Some x stop))"
           Expected = ``match`` (varRef "x" "1") [ (("None", []), stop "1"); (("Some", [ "x" ]), stop "1") ] "1"
           Line = __LINE__ } |]
      [| { Input = "(match x (None stop) (_ _ stop))"
           Expected = ``match`` (varRef "x" "1") [ (("None", []), stop "1"); (("_", [ "_" ]), stop "1") ] "1"
           Line = __LINE__ } |]
      [| { Input = "(para stop x stop)"
           Expected = interfaceParallel (stop "1") (varRef "x" "1") (stop "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(interleave stop stop)"
           Expected = interleave (stop "1") (stop "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(hide stop x)"
           Expected = hide (stop "1") (varRef "x" "1") "1"
           Line = __LINE__ } |]
      [| { Input = "(guard x stop)"
           Expected = guard (varRef "x" "1") (stop "1") "1"
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    let sexp = SexpParser.parse tc.Input |> ResultEx.get SyntaxError.format

    match parse sexp with
    | Error(err) ->
        Assert.Fail(
            $"""line %s{tc.Line}

%s{ProcSyntaxError.format err}
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
