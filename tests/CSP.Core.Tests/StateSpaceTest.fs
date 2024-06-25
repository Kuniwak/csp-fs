module CSP.Core.Tests.StateSpaceTest

open CSP.Core
open CSP.Core.Val
open CSP.Core.ValShorthand
open CSP.Core.Proc
open CSP.Core.State
open CSP.Core.ProcEval
open CSP.Core.StateSpace
open CSP.Core.Univ
open CSP.Model.Examples
open Xunit

let univCfg: UnivConfig = { NatMax = 2u; ListLenMax = 2u }
let plusCfg: ClassPlus.PlusConfig = { NatMax = 2u; ListLenMax = 2u }

let cfg: NamedSpaceConfig =
    { UnivConfig = univCfg
      ProcEvalConfig = { EvalConfig = { UnivConfig = univCfg; PlusConfig = plusCfg } } }

let formatMap (m: Map<State<'a>, (ProcId * Val list) list>) =
    m
    |> Map.toSeq
    |> Seq.map (fun (s, pvss) ->
        pvss
        |> Seq.map (fun (pn, vs) ->
            let vs = vs |> Seq.map Val.format |> String.concat " "
            $"""%s{format s} ⟹ Unwind %s{pn} %s{vs}""")
        |> String.concat "\n")
    |> String.concat "\n\n"

[<Fact>]
let lr () =
    let ns = namedSpace cfg LR.unionMap LR.ctorMap LR.procMap LR.genv in

    let expected =
        Map
            [ (InterfaceParallel(Unwind("Left", []), Set [ vUnion "sync" [] ], Unwind("Right", [])), [ ("LR", []) ])
              (Prefix(vUnion "blue" [], Prefix(vUnion "sync" [], Unwind("Left", []))), [ ("Left", []) ])
              (Prefix(vUnion "red" [], Prefix(vUnion "sync" [], Unwind("Right", []))), [ ("Right", []) ]) ]

    match ns with
    | Error(err) -> Assert.Fail(ProcEvalError.format err)
    | Ok(m) -> Assert.True((m = expected), formatMap m)

[<Fact>]
let lrh () =
    let ns = namedSpace cfg LRH.unionMap LRH.ctorMap LRH.procMap LRH.genv in

    let expected =
        Map
            [ (Hide(
                  InterfaceParallel(Unwind("Left", []), Set [ vUnion "sync" [] ], Unwind("Right", [])),
                  Set [ vUnion "sync" [] ]
               ),
               [ ("LRH", []) ])
              (Prefix(vUnion "blue" [], Prefix(vUnion "sync" [], Unwind("Left", []))), [ ("Left", []) ])
              (Prefix(vUnion "red" [], Prefix(vUnion "sync" [], Unwind("Right", []))), [ ("Right", []) ]) ]

    match ns with
    | Error(err) -> Assert.Fail(ProcEvalError.format err)
    | Ok(m) -> Assert.True((m = expected), formatMap m)
