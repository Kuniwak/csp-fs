module CSP.Core.Tests.TypeCstrTest

open Xunit
open CSP.Core.Ctor
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.Val
open CSP.Core.TypeCstr
open CSP.Core.TestUtil

let tcFmt (p: uint * TypeCstr) =
    match p with
    | n, tc -> $"({n}, {format tc})"

type ValTestCase =
    { Val: Val<string>
      Expected: (uint * TypeCstr) list }

let valTestCases: obj[] list =
    [ [| { Val = VUnit
           Expected = [ (0u, TCUnit) ] } |]
      [| { Val = VNat 0u
           Expected = [ (0u, TCNat) ] } |]
      [| { Val = VBool true
           Expected = [ (0u, TCBool) ] } |]
      [| { Val = VTuple(VBool false, VNat 0u)
           Expected = [ (0u, TCBool); (1u, TCNat); (2u, TCTuple(TCVar 0u, TCVar 1u)) ] } |]
      [| { Val = VSet(Set [])
           Expected = [ (1u, TCSet(TCVar 0u)) ] } |]
      [| { Val = VSet(Set [ VNat 0u ])
           Expected = [ (1u, TCNat); (2u, TCVar 0u); (2u, TCVar 1u); (3u, TCSet(TCVar 0u)) ] } |]
      [| { Val = VSet(Set [ VNat 0u; VNat 1u ])
           Expected =
             [ (1u, TCNat)
               (2u, TCVar 0u)
               (2u, TCVar 1u)
               (3u, TCNat)
               (4u, TCVar 0u)
               (4u, TCVar 3u)
               (5u, TCSet(TCVar 0u)) ] } |]
      [| { Val = VSet(Set [ VNat 0u; VBool false ])
           Expected =
             [ (1u, TCNat)
               (2u, TCVar 0u)
               (2u, TCVar 1u)
               (3u, TCBool)
               (4u, TCVar 0u)
               (4u, TCVar 3u)
               (5u, TCSet(TCVar 0u)) ] } |]
      [| { Val = VList([])
           Expected = [ (1u, TCList(TCVar 0u)) ] } |]
      [| { Val = VList([ VNat 0u ])
           Expected = [ (1u, TCNat); (2u, TCVar 0u); (2u, TCVar 1u); (3u, TCList(TCVar 0u)) ] } |]
      [| { Val = VList([ VNat 0u; VBool false ])
           Expected =
             [ (1u, TCNat)
               (2u, TCVar 0u)
               (2u, TCVar 1u)
               (3u, TCBool)
               (4u, TCVar 0u)
               (4u, TCVar 3u)
               (5u, TCList(TCVar 0u)) ] } |]
      [| { Val = VMap(Map.empty)
           Expected = [ (2u, TCMap(TCVar 0u, TCVar 1u)) ] } |]
      [| { Val = VMap(Map [ (VBool false, VNat 0u) ])
           Expected =
             [ (2u, TCBool)
               (3u, TCVar 0u)
               (3u, TCVar 2u)
               (4u, TCNat)
               (5u, TCVar 1u)
               (5u, TCVar 4u)
               (6u, TCMap(TCVar 0u, TCVar 1u)) ] } |]
      [| { Val = VMap(Map [ (VBool true, VNat 0u); (VUnit, VError) ])
           Expected =
             [ (2u, TCUnit)
               (3u, TCVar 0u)
               (3u, TCVar 2u)
               (4u, TCError)
               (5u, TCVar 1u)
               (5u, TCVar 4u)
               (6u, TCBool)
               (7u, TCVar 0u)
               (7u, TCVar 6u)
               (8u, TCNat)
               (9u, TCVar 1u)
               (9u, TCVar 8u)
               (10u, TCMap(TCVar 0u, TCVar 1u)) ] } |]
      [| { Val = VUnion(Ctor "Foo", VUnit)
           Expected =
             [ (0u, TCName "foo")
               (1u, TCUnit)
               (2u, TCVar 1u)
               (2u, TCUnit)
               (3u, TCUnion(TCVar 0u, TCVar 1u)) ] } |]
      [| { Val = VError
           Expected = [ (0u, TCError) ] } |] ]

[<Theory>]
[<MemberData(nameof valTestCases)>]
let typeCstrOfVal (tc: ValTestCase) =
    let init = (0u, []) in
    let cm = Map [ (Ctor "Foo", ("foo", TUnit)) ]
    let _, (_, actual) = ofVal cm init tc.Val in
    let actual = List.rev actual in

    Assert.True(tc.Expected = actual, cmp tcFmt tc.Expected actual)

type ExprTestCase =
    { Expr: Expr<string, string>
      Expected: (uint * TypeCstr) list }

let exprTestCases: obj[] list =
    [ [| { Expr = Lit VUnit
           Expected = [ (0u, TCUnit) ] } |]
         { Expr = Union (Ctor "Foo", Lit VUnit)
           Expected = [ (1u, TCVar 0); (1u, ) ] }
     ]
    
[<Theory>]
[<MemberData(nameof exprTestCases)>]
let typeCstrOfExpr (tc: ExprTestCase) =
     let init = (0u, []) in
     let cm = Map [(Ctor "Foo", ("foo", TUnit))] in
     let tenv = Map.empty in
     let _, (_, actual) = ofExpr cm tenv init tc.Expr in
     
     Assert.True(tc.Expected = actual, cmp tcFmt tc.Expected actual)
