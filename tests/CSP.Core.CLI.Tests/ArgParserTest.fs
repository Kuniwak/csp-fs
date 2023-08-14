module CSP.Core.CLI.ArgParser

open Xunit
open CSP.Core.TestUtil
open CSP.Core.CLI.ArgParser

type TestCaseOk =
    { Input: string list
      OptDecl: Map<string, OptType>
      Expected: Opt list
      Line: string }

let testCasesOk: obj[] list =
    [ [| { Input = []
           OptDecl = Map.empty
           Expected = []
           Line = __LINE__ } |]
      [| { Input = [ "a" ]
           OptDecl = Map.empty
           Expected = [ Arg "a" ]
           Line = __LINE__ } |]
      [| { Input = [ "a"; "b" ]
           OptDecl = Map.empty
           Expected = [ Arg "a"; Arg "b" ]
           Line = __LINE__ } |]
      [| { Input = [ "-a" ]
           OptDecl = Map [ ("a", OTBool) ]
           Expected = [ Opt("a", OVBool(true)) ]
           Line = __LINE__ } |]
      [| { Input = [ "-a"; "123" ]
           OptDecl = Map [ ("a", OTBool) ]
           Expected = [ Opt("a", OVBool(true)); Arg "123" ]
           Line = __LINE__ } |]
      [| { Input = [ "-a"; "123" ]
           OptDecl = Map [ ("a", OTNat) ]
           Expected = [ Opt("a", OVNat(123u)) ]
           Line = __LINE__ } |]
      [| { Input = [ "-a"; "123"; "-b" ]
           OptDecl = Map [ ("a", OTNat); ("b", OTBool) ]
           Expected = [ Opt("a", OVNat(123u)); Opt("b", OVBool(true)) ]
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCasesOk)>]
let ok (tc: TestCaseOk) =
    match parseArgs tc.OptDecl tc.Input with
    | Error(err) ->
        Assert.Fail
            $"""line at %s{tc.Line}

Error: %s{err}"""
    | Ok(actual) ->
        let cmp = cmp (fun opt -> $"%A{opt}") in

        Assert.True(
            tc.Expected = actual,
            $"""line at %s{tc.Line}

%s{cmp tc.Expected actual}
"""
        )

type TestCaseNg =
    { NGInput: string list
      OptDecl: Map<string, OptType>
      Line: string }

let testCasesNg: obj[] list =
    [ [| { NGInput = [ "-a" ]
           OptDecl = Map.empty
           Line = __LINE__ } |]
      [| { NGInput = [ "-a" ]
           OptDecl = Map [ ("a", OTNat) ]
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof testCasesNg)>]
let ng (tc: TestCaseNg) =
    match parseArgs tc.OptDecl tc.NGInput with
    | Error _ -> ()
    | Ok(actual) ->
        Assert.Fail
            $"""line at %s{tc.Line}

Error: %A{actual}"""
