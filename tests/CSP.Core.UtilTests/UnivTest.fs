module CSP.Core.UtilTests.UnivTest

open Xunit
open CSP.Core.Util
open CSP.Core.TestUtil

type OfListTestCase =
    { Input: string list
      Len: uint
      Expected: string list list }

let ofListTestCases: obj[] list =
    [ [| { Input = [ "a" ]
           Len = 0u
           Expected = [ [] ] } |]
      [| { Input = [ "a" ]
           Len = 1u
           Expected = [ [ "a" ] ] } |]
      [| { Input = [ "a" ]
           Len = 2u
           Expected = [ [ "a"; "a" ] ] } |]
      [| { Input = [ "a"; "b" ]
           Len = 0u
           Expected = [ [] ] } |]
      [| { Input = [ "a"; "b" ]
           Len = 1u
           Expected = [ [ "a" ]; [ "b" ] ] } |]
      [| { Input = [ "a"; "b" ]
           Len = 2u
           Expected = [ [ "a"; "a" ]; [ "a"; "b" ]; [ "b"; "a" ]; [ "b"; "b" ] ] } |] ]

[<Theory>]
[<MemberData(nameof ofListTestCases)>]
let testOfList (testCase: OfListTestCase) =
    let actual = Univ.ofList testCase.Len testCase.Input in
    Assert.True(testCase.Expected = actual, cmp (fun x -> $"%A{x}") testCase.Expected actual)

type OfMapTestCase =
    { Keys: string list
      Values: string list
      Expected: Map<string, string> list }

let ofMapTestCases: obj[] list =
    [ [| { Keys = [ "k" ]
           Values = [ "v" ]
           Expected = [ Map.empty; Map [ ("k", "v") ] ] } |]
      [| { Keys = [ "k" ]
           Values = [ "v1"; "v2" ]
           Expected = [ Map.empty; Map [ ("k", "v1") ]; Map [ ("k", "v2") ] ] } |]
      [| { Keys = [ "k1"; "k2" ]
           Values = [ "v" ]
           Expected =
             [ Map.empty
               Map [ ("k1", "v") ]
               Map [ ("k2", "v") ]
               Map [ ("k1", "v"); ("k2", "v") ] ] } |]
      [| { Keys = [ "k1"; "k2" ]
           Values = [ "v1"; "v2" ]
           Expected =
             [ Map.empty
               Map [ ("k1", "v1") ]
               Map [ ("k1", "v2") ]
               Map [ ("k2", "v1") ]
               Map [ ("k2", "v2") ]
               Map [ ("k1", "v1"); ("k2", "v1") ]
               Map [ ("k1", "v1"); ("k2", "v2") ]
               Map [ ("k1", "v2"); ("k2", "v1") ]
               Map [ ("k1", "v2"); ("k2", "v2") ] ] } |] ]

[<Theory>]
[<MemberData(nameof ofMapTestCases)>]
let testOfMap (testCase: OfMapTestCase) =
    let actual = Univ.ofMap testCase.Keys testCase.Values in
    Assert.True(testCase.Expected = actual, cmp (fun x -> $"%A{x}") testCase.Expected actual)
