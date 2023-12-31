module CSP.Core.ProcTypeInferenceTest

open CSP.Core.Util
open Xunit
open CSP.Core
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.Proc
open CSP.Core.ExprShorthand
open CSP.Core.ProcShorthand
open CSP.Core.TypeShorthand
open CSP.Core.TypeCstrShorthand
open CSP.Core.TypeInferenceState
open CSP.Core.ProcTypeInference

type ProcTestCaseOk =
    { Proc: Proc<unit>
      Expected: Type list
      Line: string }

let procTestCasesOk: obj[] list =
    [ [| { Proc = unwind "Foo" [] __LINE__
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = unwind "Bar" [ litUnit __LINE__ ] __LINE__
           Expected = [ tUnit ]
           Line = __LINE__ } |]
      [| { Proc = stop __LINE__
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = skip __LINE__
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = prefix (litUnit __LINE__) (stop __LINE__) __LINE__
           Expected = [ tUnit ]
           Line = __LINE__ } |]
      [| { Proc =
             prefixRecv
                 (setInsert (litNat 0u __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__)
                 "x"
                 (stop __LINE__)
                 __LINE__
           Expected = [ tSet tNat ]
           Line = __LINE__ } |]
      [| { Proc = intCh (stop __LINE__) (stop __LINE__) __LINE__
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = extCh (stop __LINE__) (stop __LINE__) __LINE__
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = seq (stop __LINE__) (stop __LINE__) __LINE__
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = ``if`` (litTrue __LINE__) (stop __LINE__) (stop __LINE__) __LINE__
           Expected = [ tBool ]
           Line = __LINE__ } |]
      [| { Proc = ``if`` (varRef "GLOBAL" __LINE__) (stop __LINE__) (stop __LINE__) __LINE__
           Expected = [ tBool ]
           Line = __LINE__ } |]
      [| { Proc =
             ``match``
                 (ctor "Some" [ litUnit __LINE__ ] __LINE__)
                 [ (("Some", [ "x" ]), stop __LINE__); (("None", []), stop __LINE__) ]
                 __LINE__
           Expected = [ tUnion "option" [ tUnit ] ]
           Line = __LINE__ } |]
      [| { Proc =
             ``match``
                 (ctor "Some" [ varRef "GLOBAL" __LINE__ ] __LINE__)
                 [ (("Some", [ "x" ]), stop __LINE__); (("None", []), stop __LINE__) ]
                 __LINE__
           Expected = [ tUnion "option" [ tBool ] ]
           Line = __LINE__ } |]
      [| { Proc = interfaceParallel (stop __LINE__) (litEmpty (tSet tUnit) __LINE__) (stop __LINE__) __LINE__
           Expected = [ tSet tUnit ]
           Line = __LINE__ } |]
      [| { Proc = interleave (stop __LINE__) (stop __LINE__) __LINE__
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = hide (stop __LINE__) (litEmpty (tSet tUnit) __LINE__) __LINE__
           Expected = [ tSet tUnit ]
           Line = __LINE__ } |]
      [| { Proc = guard (litEmpty (tSet tUnit) __LINE__) (stop __LINE__) __LINE__
           Expected = [ tSet tUnit ]
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof procTestCasesOk)>]
let inferProcOk (tc: ProcTestCaseOk) =
    let um = UnionMap.builtin
    let cm = CtorMap.from um |> ResultEx.getValue CtorMapError.format in
    let tcenv = TypeCstrEnv.from [ ("GLOBAL", tcBool) ] in

    let pm =
        ProcMap.from [ (("Foo", []), stop __LINE__); (("Bar", [ ("x", tUnit) ]), stop __LINE__) ]
        |> ResultEx.getValue ProcMapError.format

    match postProcess (infer pm um cm tcenv tc.Proc init) with
    | Ok(p, s) ->
        let sep = ", "

        Assert.True(
            tc.Expected = get p,
            $"""line %s{tc.Line}

Expected: %s{String.concat sep (List.map Type.format tc.Expected)}
Actual:   %s{String.concat sep (List.map Type.format (get p))}
Inferred as:
%s{format typeAnnotation p}

TVar mapping:
%s{TypeCstrUncertainVar.format s.UncertainVarMap}
"""
        )
    | Error terr ->
        Assert.Fail
            $"""line %s{tc.Line}

Proc:
%s{format noAnnotation tc.Proc}

Expected: (no error)
Actual:   %s{TypeError.format terr}
"""
