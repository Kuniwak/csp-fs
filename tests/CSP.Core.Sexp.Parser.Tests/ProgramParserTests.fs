module CSP.Core.Sexp.ProgramParser

open CSP.Core
open CSP.Core.Util
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.LineNum
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.UnionMap
open CSP.Core.ExprShorthand
open CSP.Core.ProcShorthand
open CSP.Core.Type
open CSP.Core.Sexp
open CSP.Core.Sexp.ProgramParser
open CSP.Core.TestUtil
open Xunit

type TestCase =
    { Input: string
      ExpectedProcMap: ((string * (string * Type) list) * Proc<unit>) list
      ExpectedUnionMap: ((TVarId list * UnionName) * (string * Type list) list) list
      ExpectedCtorMap: (string * string) list
      ExpectedGlobalEnv: (string * Expr<unit>) list
      Line: LineNum }

let testCases: obj[] list =
    [ [| { Input =
             """
(proc P () stop)
"""
           ExpectedProcMap = [ (("P", []), stop "2") ]
           ExpectedUnionMap = []
           ExpectedCtorMap = []
           ExpectedGlobalEnv = []
           Line = __LINE__ } |]
      [| { Input =
             """
(type event A)

(proc P () (prefix A stop))
"""
           ExpectedProcMap = [ (("P", []), (prefix (ctor "A" [] "4") (stop "4")) "4") ]
           ExpectedUnionMap = [ (([], "event"), [ ("A", []) ]) ]
           ExpectedCtorMap = [ ("A", "event") ]
           ExpectedGlobalEnv = []
           Line = __LINE__ } |]
      [| { Input =
             """
(const x 0)

(proc P () (prefix x stop))
"""
           ExpectedProcMap = [ (("P", []), (prefix (varRef "x" "4") (stop "4")) "4") ]
           ExpectedUnionMap = []
           ExpectedCtorMap = []
           ExpectedGlobalEnv = [ ("x", litNat 0u "2") ]
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    let expectedUnionMap =
        from tc.ExpectedUnionMap |> ResultEx.getValue UnionMapError.format in

    let expectedProcMap =
        ProcMap.from tc.ExpectedProcMap |> ResultEx.getValue ProcMapError.format in

    let builtin =
        [ ("Left", "either")
          ("Right", "either")
          ("None", "option")
          ("Some", "option") ]

    let expectedCtorMap =
        CtorMap(Map([ for ctor, un in tc.ExpectedCtorMap @ builtin -> (Ctor ctor, un) ])) in

    let expectedGenv =
        GlobalEnv.from tc.ExpectedGlobalEnv |> ResultEx.getValue GlobalEnvError.format in

    match parse tc.Input with
    | Error(err) ->
        Assert.Fail(
            $"""line %s{tc.Line}

Error: %s{ProgramSyntaxError.format err}
"""
        )
    | Ok(actualProcMap, actualUnionMap, actualCtorMap, actualGenv) ->

        Assert.True(
            (expectedUnionMap = actualUnionMap),
            $"""line %s{tc.Line}

%s{cmp formatEntry (toSeq expectedUnionMap) (toSeq actualUnionMap)}
"""

        )

        Assert.True(
            (expectedProcMap = actualProcMap),
            $"""line %s{tc.Line}

%s{cmp (ProcMap.formatEntry noAnnotation) (ProcMap.toSeq expectedProcMap) (ProcMap.toSeq actualProcMap)}
"""
        )

        Assert.True(
            (expectedCtorMap = actualCtorMap),
            $"""line %s{tc.Line}

%s{cmp CtorMap.formatEntry (CtorMap.toSeq expectedCtorMap) (CtorMap.toSeq actualCtorMap)}
"""
        )

        Assert.True(
            (expectedCtorMap = actualCtorMap),
            $"""line %s{tc.Line}

%s{cmp GlobalEnv.formatEntry (GlobalEnv.toSeq expectedGenv) (GlobalEnv.toSeq actualGenv)}
"""
        )
