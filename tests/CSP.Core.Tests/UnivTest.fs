module CSP.Core.Tests.UnivTest

open CSP.Core
open CSP.Core.Util
open Xunit
open CSP.Core.LineNum
open CSP.Core.Univ
open CSP.Core.Val
open CSP.Core.ValShorthand
open CSP.Core.Type
open CSP.Core.TypeShorthand
open CSP.Core.TestUtil

let natMax = 2u
let listMax = 2u
let cfg = univConfig natMax listMax

type TestCase =
    { Config: UnivConfig
      Type: Type
      UnionMap: ((TVarId list * UnionName) * (string * Type list) seq) seq
      Expected: Set<Val>
      Line: LineNum }

let testCases: obj[] list =
    [ [| { Config = cfg
           Type = tUnit
           UnionMap = []
           Expected = Set [ vUnit ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tBool
           UnionMap = []
           Expected = Set [ vBool true; vBool false ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tNat
           UnionMap = []
           Expected = Set [ vNat 0u; vNat 1u ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tTuple2 tBool tBool
           UnionMap = []
           Expected =
             Set
                 [ vTuple2 (vBool false) (vBool false)
                   vTuple2 (vBool false) (vBool true)
                   vTuple2 (vBool true) (vBool false)
                   vTuple2 (vBool true) (vBool true) ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tUnion "single" []
           UnionMap = [ (([], "single"), [ ("Single", []) ]) ]
           Expected = Set [ vUnion "Single" [] ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tUnion "single" []
           UnionMap = [ (([], "single"), [ ("Single", [ tBool ]) ]) ]
           Expected = Set [ vUnion "Single" [ vBool true ]; vUnion "Single" [ vBool false ] ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tUnion "single" []
           UnionMap = [(([], "single"), [ ("Single", [ tBool; tBool ]) ])] 
           Expected =
             Set
                 [ vUnion "Single" [ vBool false; vBool false ]
                   vUnion "Single" [ vBool false; vBool true ]
                   vUnion "Single" [ vBool true; vBool false ]
                   vUnion "Single" [ vBool true; vBool true ] ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tSet tBool
           UnionMap = [] 
           Expected =
             Set
                 [ vSet []
                   vSet [ vBool true ]
                   vSet [ vBool false ]
                   vSet [ vBool true; vBool false ] ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tSet tBool
           UnionMap = [] 
           Expected =
             Set
                 [ vSet []
                   vSet [ vBool true ]
                   vSet [ vBool false ]
                   vSet [ vBool true; vBool false ] ]
           Line = __LINE__ } |]
      [| { Config = cfg
           Type = tList tBool
           UnionMap = [] 
           Expected =
             Set
                 [ vList []
                   vList [ vBool true ]
                   vList [ vBool false ]
                   vList [ vBool false; vBool false ]
                   vList [ vBool false; vBool true ]
                   vList [ vBool true; vBool false ]
                   vList [ vBool true; vBool true ] ]
           Line = __LINE__ } |]
      [| { Config = cfg
           UnionMap = [] 
           Type = tMap tBool tBool
           Expected =
             Set
                 [ vMap []
                   vMap [ (vBool false, vBool false) ]
                   vMap [ (vBool false, vBool true) ]
                   vMap [ (vBool true, vBool false) ]
                   vMap [ (vBool true, vBool true) ]
                   vMap [ (vBool true, vBool false); (vBool false, vBool false) ]
                   vMap [ (vBool true, vBool false); (vBool false, vBool true) ]
                   vMap [ (vBool true, vBool true); (vBool false, vBool false) ]
                   vMap [ (vBool true, vBool true); (vBool false, vBool true) ] ]
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCases)>]
let testUniv (tc: TestCase) : unit =
    let um = ResultEx.get UnionMapError.format (UnionMap.from tc.UnionMap)
    let vRes = univ tc.Config um tc.Type in

    match vRes with
    | Error(err) ->
        Assert.Fail(
            $"""line %s{tc.Line}

Error: %s{UnivError.format err}"""
        )
    | Ok(actual) ->
        let actual = Set.ofList actual

        Assert.True(
            (actual = tc.Expected),
            $"""line %s{tc.Line}

%s{cmp Val.format (Set.toList tc.Expected) (Set.toList actual)}"""
        )
