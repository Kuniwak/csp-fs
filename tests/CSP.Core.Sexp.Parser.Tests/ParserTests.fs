module CSP.Core.Sexp.Parser

open FSharpPlus
open CSP.Core
open CSP.Core.Util
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.UnionMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.Type
open CSP.Core.Sexp
open CSP.Core.Sexp.ProgramParser
open CSP.Core.TestUtil
open Xunit

type TestCase =
    { Input: string
      ExpectedProcMap: ((string * (string * Type) list) * Proc<unit>) seq
      ExpectedUnionMap: ((TVarId list * UnionName) * (string * Type list) seq) seq
      ExpectedInit: ProcId * Expr<unit> list }

let testCases: obj[] list =
    [ [| { Input =
             """
(def P () (prefix 0 (unwind P ()))
(init P ())
"""
           ExpectedProcMap = [ (("P", []), prefix (litNat 0u "1") (unwind "P" [] "1") "1") ]
           ExpectedUnionMap = []
           ExpectedInit = ("P", []) } |]
      [| { Input =
             """
(def P (x) (prefix x (unwind P (- x 1)))
(init P (5))
"""
           ExpectedProcMap = [ (("P", []), prefix (litNat 0u "1") (unwind "P" [] "1") "1") ]
           ExpectedUnionMap = []
           ExpectedInit = ("P", [ litNat 5u "2" ]) } |]
      [| { Input =
             """
(def P (x y) (prefix (tuple x y)) (unwind P (- x 1) (- y 1)))
(init P (3 2))
"""
           ExpectedProcMap = [ (("P", []), prefix (litNat 0u "1") (unwind "P" [] "1") "1") ]
           ExpectedUnionMap = []
           ExpectedInit = ("P", [ litNat 3u "2"; litNat 2u "2" ]) } |]
      [| { Input =
             """
(type () event (A ()))

(def P () stop) 
(init P ())
"""
           ExpectedProcMap = [ (("P", []), stop "3") ]
           ExpectedUnionMap = [ (([], "A"), []) ]
           ExpectedInit = ("P", []) } |]
      [| { Input =
             """
(type () eventA (A ()))
(type () eventB (B ()))

(def P () stop) 
(init P ())
"""
           ExpectedProcMap = [ (("P", []), stop "3") ]
           ExpectedUnionMap = [ (([], "A"), []); (([], "B"), []) ]
           ExpectedInit = ("P", []) } |] ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    match ProcMap.from tc.ExpectedProcMap with
    | Error(err) -> Assert.Fail(ProcMapError.format err)
    | Ok(expectedProcMap) ->
        match parse (String.trimStartWhiteSpaces tc.Input) with
        | Error(err) -> Assert.Fail(SyntaxError.format err)
        | Ok(actualProcMap, actualUnionMap) ->
            let expectedUnionMap = from tc.ExpectedUnionMap |> ResultEx.get UnionMapError.format

            Assert.True(
                (expectedUnionMap = actualUnionMap),
                cmp formatEntry (toSeq expectedUnionMap) (toSeq actualUnionMap)
            )

            Assert.True(
                (expectedProcMap = actualProcMap),
                cmp ProcMap.formatEntry (ProcMap.toSeq expectedProcMap) (ProcMap.toSeq actualProcMap)
            )
