module CSP.Core.UtilTests.SetExTest

open CSP.Core.Util
open CSP.Core.TestUtil
open Xunit

type Cartesian2TestCase =
    { Input1: string list
      Input2: string list
      Expected: (string * string) list }

let cartesian2TestCases: obj[] list =
    [ [| { Input1 = []
           Input2 = []
           Expected = [] } |]
      [| { Input1 = [ "a" ]
           Input2 = []
           Expected = [] } |]
      [| { Input1 = []
           Input2 = [ "b" ]
           Expected = [] } |]
      [| { Input1 = [ "a" ]
           Input2 = [ "b" ]
           Expected = [ ("a", "b") ] } |]
      [| { Input1 = [ "a1"; "a2" ]
           Input2 = [ "b" ]
           Expected = [ ("a1", "b"); ("a2", "b") ] } |]
      [| { Input1 = [ "a" ]
           Input2 = [ "b1"; "b2" ]
           Expected = [ ("a", "b1"); ("a", "b2") ] } |]
      [| { Input1 = [ "a1"; "a2" ]
           Input2 = [ "b1"; "b2" ]
           Expected = [ ("a1", "b1"); ("a1", "b2"); ("a2", "b1"); ("a2", "b2") ] } |] ]

[<Theory>]
[<MemberData(nameof cartesian2TestCases)>]
let testCartesian2 (testCase: Cartesian2TestCase) =
    let actual = ListEx.cartesian2 testCase.Input1 testCase.Input2 in
    Assert.True(testCase.Expected = actual, cmp (fun x -> $"%A{x}") testCase.Expected actual)


type CartesianTestCase =
    { Input: string list list
      Expected: string list list }

let cartesianTestCases: obj[] list =
    [ [| { Input = []; Expected = [ [] ] } |]
      [| { Input = [ [] ]; Expected = [] } |]
      [| { Input = [ [ "a" ] ]
           Expected = [ [ "a" ] ] } |]
      [| { Input = [ [ "a1"; "a2" ] ]
           Expected = [ [ "a1" ]; [ "a2" ] ] } |]
      [| { Input = [ []; [] ]; Expected = [] } |]
      [| { Input = [ [ "a" ]; [] ]
           Expected = [] } |]
      [| { Input = [ []; [ "b" ] ]
           Expected = [] } |]
      [| { Input = [ [ "a" ]; [ "b" ] ]
           Expected = [ [ "a"; "b" ] ] } |]
      [| { Input = [ [ "a1"; "a2" ]; [ "b1"; "b2" ] ]
           Expected = [ [ "a1"; "b1" ]; [ "a2"; "b1" ]; [ "a1"; "b2" ]; [ "a2"; "b2" ] ] } |] ]

[<Theory>]
[<MemberData(nameof cartesianTestCases)>]
let testCartesian (testCase: CartesianTestCase) =
    let actual = ListEx.cartesian testCase.Input in
    Assert.True(testCase.Expected = actual, cmp (fun x -> $"%A{x}") testCase.Expected actual)

type PowerTestCase =
    { Input: string list
      Expected: string list list }

let powerTestCases: obj[] list =
    [ [| { Input = []; Expected = [ [] ] } |]
      [| { Input = [ "a" ]
           Expected = [ []; [ "a" ] ] } |]
      [| { Input = [ "a"; "b" ]
           Expected = [ []; [ "a" ]; [ "b" ]; [ "a"; "b" ] ] } |] ]

[<Theory>]
[<MemberData(nameof powerTestCases)>]
let testPower (testCase: PowerTestCase) =
    let actual = ListEx.power testCase.Input in
    Assert.True(testCase.Expected = actual, cmp (fun x -> $"%A{x}") testCase.Expected actual)
