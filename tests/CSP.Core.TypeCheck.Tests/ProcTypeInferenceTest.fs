module CSP.Core.ProcTypeInferenceTest

open Xunit
open CSP.Core
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.Proc
open CSP.Core.ExprShorthand
open CSP.Core.ProcShorthand
open CSP.Core.TypeShorthand
open CSP.Core.ProcTypeInference
open CSP.Core.Util

type ProcTestCaseOk =
    { Proc: Proc<unit>
      Expected: Type list
      Line: string }

let procTestCasesOk: obj[] list =
    [ [| { Proc = unwind "Foo" []
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = unwind "Foo" [ litUnit ]
           Expected = [ tUnit ]
           Line = __LINE__ } |]
      [| { Proc = stop
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = skip
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = prefix litUnit stop
           Expected = [ tUnit ]
           Line = __LINE__ } |]
      [| { Proc = prefixRecv (setInsert (litNat 0u) (litEmpty (tSet tNat))) "x" stop
           Expected = [ tSet tNat ]
           Line = __LINE__ } |]
      [| { Proc = intCh stop stop
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = extCh stop stop
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = seq stop stop
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = ``if`` litTrue stop stop
           Expected = [ tBool ]
           Line = __LINE__ } |]
      [| { Proc = ``if`` (varRef "GLOBAL") stop stop
           Expected = [ tBool ]
           Line = __LINE__ } |]
      [| { Proc = ``match`` (ctor "Some" [ litUnit ]) [ ((Some "Some", [ "x" ]), stop) ]
           Expected = [ tUnion "option" [ ("Some", [ tUnit ]); ("None", []) ] ]
           Line = __LINE__ } |]
      [| { Proc = ``match`` (ctor "Some" [ varRef "GLOBAL" ]) [ ((Some "Some", [ "x" ]), stop) ]
           Expected = [ tUnion "option" [ ("Some", [ tBool ]); ("None", []) ] ]
           Line = __LINE__ } |]
      [| { Proc = interfaceParallel stop (litEmpty (tSet tUnit)) stop
           Expected = [ tSet tUnit ]
           Line = __LINE__ } |]
      [| { Proc = interleave stop stop
           Expected = []
           Line = __LINE__ } |]
      [| { Proc = hide stop (litEmpty (tSet tUnit))
           Expected = [ tSet tUnit ]
           Line = __LINE__ } |]
      [| { Proc = guard (litEmpty (tSet tUnit)) stop
           Expected = [ tSet tUnit ]
           Line = __LINE__ } |] ]

[<Theory>]
[<MemberData(nameof procTestCasesOk)>]
let inferProcOk (tc: ProcTestCaseOk) =
    let tOption = tUnion "option" [ ("Some", [ tVar 0u ]); ("None", []) ] in
    let tFoo = tUnion "foo" [ ("Foo", []) ] in
    let cm = CtorMap.from [ tOption; tFoo ] in

    let tenv, s =
        ResultEx.get TypeEnvError.format (TypeInferenceState.from (TypeEnv.from [ ("GLOBAL", tBool) ])) in

    match postProcess (infer cm tenv tc.Proc s) with
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
