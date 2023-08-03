module CSP.Core.Sexp.Parser

open CSP.Core.Expr
open FSharpPlus
open CSP.Core
open CSP.Core.Proc
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.Type
open CSP.Core.TypeShorthand
open CSP.Core.Sexp
open CSP.Core.Sexp.Parser
open CSP.Core.TestUtil
open Xunit

type TestCase =
    { Input: string
      ExpectedProcMap: ((string * (string * Type) list) * Proc<unit>) seq
      ExpectedTypes: Type list
      ExpectedInit: ProcId * Expr<unit> list }

let testCases: obj[] list =
    [ [| { Input =
             """
(def P () (prefix 0 (unwind P ()))
(init P ())
"""
           ExpectedProcMap = [ (("P", []), prefix (litNat 0u "1") (unwind "P" [] "1") "1") ]
           ExpectedTypes = []
           ExpectedInit = ("P", []) } |]
      [| { Input =
             """
(def P (x) (prefix x (unwind P (- x 1)))
(init P (5))
"""
           ExpectedProcMap = [ (("P", []), prefix (litNat 0u "1") (unwind "P" [] "1") "1") ]
           ExpectedTypes = []
           ExpectedInit = ("P", [ litNat 5u "2" ]) } |]
      [| { Input =
             """
(def P (x y) (prefix (tuple x y)) (unwind P (- x 1) (- y 1)))
(init P (3 2))
"""
           ExpectedProcMap = [ (("P", []), prefix (litNat 0u "1") (unwind "P" [] "1") "1") ]
           ExpectedTypes = []
           ExpectedInit = ("P", [ litNat 3u "2"; litNat 2u "2" ]) } |]
      [| { Input =
             """
(define-type event (A ()))

(define-proc P () stop) 
(init P ())
"""
           ExpectedProcMap = [ (("P", []), stop "3") ]
           ExpectedTypes = [tUnion "event" [("A", [])]]
           ExpectedInit = ("P", []) } |]
      [| { Input =
             """
(type event (A ()) (B ()))

(def P () stop) 
(init P ())
"""
           ExpectedProcMap = [ (("P", []), stop "3") ]
           ExpectedTypes = [tUnion "event" [("A", []); ("B", [])]]
           ExpectedInit = ("P", []) } |]
      [| { Input =
             """
(type event (A (nat unit)))

(def P () stop) 
(init P ())
"""
           ExpectedProcMap = [ (("P", []), stop "3") ]
           ExpectedTypes = [tUnion "event" [("A", [tNat; tUnit])]]
           ExpectedInit = ("P", []) } |]
       ]

[<Theory>]
[<MemberData(nameof testCases)>]
let parseTest (tc: TestCase) =
    match ProcMap.from tc.ExpectedProcMap with
    | Error(err) -> Assert.Fail(ProcMapError.format err)
    | Ok(expectedProcMap) ->
        match parse (String.trimStartWhiteSpaces tc.Input) with
        | Error(err) -> Assert.Fail(SyntaxError.format err)
        | Ok(actualProcMap, actualTypes) ->
            Assert.True(tc.ExpectedTypes = actualTypes, cmp (format true) tc.ExpectedTypes actualTypes)

            Assert.True(
                (expectedProcMap = actualProcMap),
                cmp ProcMap.formatEntry (ProcMap.toList expectedProcMap) (ProcMap.toList actualProcMap)
            )
