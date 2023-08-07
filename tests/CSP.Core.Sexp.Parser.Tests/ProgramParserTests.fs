module CSP.Core.Sexp.ProgramParser

open CSP.Core
open CSP.Core.Util
open CSP.Core.LineNum
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.UnionMap
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
      ExpectedInit: ProcId * Expr<unit> list
      Line: LineNum }

let testCases: obj[] list =
    [ [| { Input =
             """
(def P () stop)
(init P ())
"""
           ExpectedProcMap = [ (("P", []), stop "2") ]
           ExpectedUnionMap = []
           ExpectedInit = ("P", [])
           Line = __LINE__ } |]
      [| { Input =
             """
(type () event A)

(def P () stop)
(init P ())
"""
           ExpectedProcMap = [ (("P", []), stop "4") ]
           ExpectedUnionMap = [ (([], "event"), [ ("A", []) ]) ]
           ExpectedInit = ("P", [])
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    match ProcMap.from tc.ExpectedProcMap with
    | Error(err) -> Assert.Fail(ProcMapError.format err)
    | Ok(expectedProcMap) ->
        match parse tc.Input with
        | Error(err) ->
            Assert.Fail(
                $"""line %s{tc.Line}

Error: %s{ProgramSyntaxError.format err}
"""
            )
        | Ok(actualProcMap, actualUnionMap, actualInit) ->
            let expectedUnionMap = from tc.ExpectedUnionMap |> ResultEx.get UnionMapError.format

            Assert.True(
                (expectedUnionMap = actualUnionMap),
                $"""line %s{tc.Line}

%s{cmp formatEntry (toSeq expectedUnionMap) (toSeq actualUnionMap)}
"""

            )

            Assert.True(
                (expectedProcMap = actualProcMap),
                $"""line %s{tc.Line}

%s{cmp ProcMap.formatEntry (ProcMap.toSeq expectedProcMap) (ProcMap.toSeq actualProcMap)}
"""
            )

            Assert.Equal(tc.ExpectedInit, actualInit)
