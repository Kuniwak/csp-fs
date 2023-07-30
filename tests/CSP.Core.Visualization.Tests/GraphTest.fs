module CSP.Core.Tests.GraphTests

open CSP.Core.ProcEvalError
open CSP.Core.Univ
open Xunit
open CSP.Core
open CSP.Core.Util
open CSP.Core.ProcEval
open CSP.Core.Trans
open CSP.Core.Search
open CSP.Core.TypeShorthand
open CSP.Core.ExprShorthand
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.Eval
open CSP.Core.ValShorthand
open CSP.Core.Visualization.DotLang

let univCfg: UnivConfig = { NatMax = 5u; ListLenMax = 3u }

let procEvalCfg: ProcEvalConfig =
    { EvalConfig = { UnivConfig = univCfg }
      MaxUnwind = 100 }

let dotCfg: DotConfig =
    { GraphConfig =
        { TransConfig = { ProcEvalConfig = procEvalCfg }
          ProcEvalConfig = procEvalCfg
          SearchConfig = { NodeMax = 1000 }
          NamedConfig =
            { ProcEvalConfig = procEvalCfg
              UnivConfig = univCfg } } }

let dot = dot dotCfg

[<Fact>]
let abSkip () =
    let tEvent = tUnion "event" [ ("a", []); ("b", []) ] in

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ ("ABSkip", []), seq (unwind "ASkip" [] __LINE__) (unwind "BSkip" [] __LINE__) __LINE__
                  ("ASkip", []), prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__
                  ("BSkip", []), prefix (ctor "b" [] __LINE__) (skip __LINE__) __LINE__ ]) in

    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ]) in
    let genv = Env.empty in

    match dot pm cm genv "ABSkip" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->
        Assert.True(
            """digraph G {
  "Ω"
  "SKIP"
  "(b -> SKIP env={})"
  "(SKIP ; BSkip)"
  "(ASkip ; BSkip)"
  "SKIP" -> "Ω" [label="✓"]
  "(b -> SKIP env={})" -> "SKIP" [label="b"]
  "(SKIP ; BSkip)" -> "(b -> SKIP env={})" [label="τ"]
  "(ASkip ; BSkip)" -> "(SKIP ; BSkip)" [label="a"]
}""" =
                actual,
            actual
        )

[<Fact>]
let parABC () =
    let tEvent = tUnion "event" [ ("a", []); ("b", []); ("c", []); ("d", []) ] in

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("ParABC", []),
                   interleave
                       (prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__)
                       (interleave
                           (prefix (ctor "b" [] __LINE__) (skip __LINE__) __LINE__)
                           (prefix (ctor "c" [] __LINE__) (skip __LINE__) __LINE__)
                           __LINE__)
                       __LINE__)
                  (("P", []),
                   seq (unwind "ParABC" [] __LINE__) (prefix (ctor "d" [] __LINE__) (skip __LINE__) __LINE__) __LINE__) ]) in

    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ]) in
    let genv = Env.empty in

    match dot pm cm genv "P" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "Ω"
  "SKIP"
  "(d -> SKIP env={})"
  "((Ω ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(ParABC ; (d -> SKIP env={}))"
  "SKIP" -> "Ω" [label="✓"]
  "(d -> SKIP env={})" -> "SKIP" [label="d"]
  "((Ω ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" -> "(d -> SKIP env={})" [label="τ"]
  "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (Ω ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(ParABC ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ (SKIP ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(ParABC ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(ParABC ; (d -> SKIP env={}))" -> "((SKIP ⟦(() set).empty⟧ ((b -> SKIP env={}) ⟦(() set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
}""" =
                actual,
            actual
        )

[<Fact>]
let rand2 () =
    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("P", []),
                   intCh
                       (prefix (litNat 1u __LINE__) (unwind "P" [] __LINE__) __LINE__)
                       (prefix (litNat 2u __LINE__) (unwind "P" [] __LINE__) __LINE__)
                       __LINE__) ]) in

    let cm = CtorMap.empty
    let genv = Env.empty in

    match dot pm cm genv "P" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->
        Assert.True(
            """digraph G {
  "(2 -> P env={})"
  "(1 -> P env={})"
  "((1 -> P env={}) ⨅ (2 -> P env={}))"
  "(2 -> P env={})" -> "((1 -> P env={}) ⨅ (2 -> P env={}))" [label="2"]
  "(1 -> P env={})" -> "((1 -> P env={}) ⨅ (2 -> P env={}))" [label="1"]
  "((1 -> P env={}) ⨅ (2 -> P env={}))" -> "(1 -> P env={})" [label="τ"]
  "((1 -> P env={}) ⨅ (2 -> P env={}))" -> "(2 -> P env={})" [label="τ"]
}""" =
                actual,
            actual
        )

[<Fact>]
let abs () =
    let tEvent = tUnion "event" [ ("a", []); ("b", []); ("s", []) ] in

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("ABS", []),
                   (extCh
                       (intCh
                           (prefix (ctor "a" [] __LINE__) (unwind "ABS" [] __LINE__) __LINE__)
                           (prefix (ctor "b" [] __LINE__) (unwind "ABS" [] __LINE__) __LINE__)
                           __LINE__)
                       (prefix (ctor "s" [] __LINE__) (stop __LINE__) __LINE__)
                       __LINE__)) ]) in

    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])
    let genv = Env.empty in

    match dot pm cm genv "ABS" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->
        Assert.True(
            """digraph G {
  "STOP"  [fillcolor=red, style=filled, fontcolor=white]
  "((b -> ABS env={}) □ (s -> STOP env={}))"
  "((a -> ABS env={}) □ (s -> STOP env={}))"
  "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))"
  "((b -> ABS env={}) □ (s -> STOP env={}))" -> "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" [label="b"]
  "((b -> ABS env={}) □ (s -> STOP env={}))" -> "STOP" [label="s"]
  "((a -> ABS env={}) □ (s -> STOP env={}))" -> "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" [label="a"]
  "((a -> ABS env={}) □ (s -> STOP env={}))" -> "STOP" [label="s"]
  "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" -> "((a -> ABS env={}) □ (s -> STOP env={}))" [label="τ"]
  "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" -> "((b -> ABS env={}) □ (s -> STOP env={}))" [label="τ"]
  "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" -> "STOP" [label="s"]
}""" =
                actual,
            actual
        )

[<Fact>]
let lr () =
    let tEvent = tUnion "event" [ ("blue", []); ("red", []); ("sync", []) ] in

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("LR", []),
                   (interfaceParallel
                       (unwind "Left" [] __LINE__)
                       (setInsert (ctor "sync" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
                       (unwind "Right" [] __LINE__))
                       __LINE__)
                  (("Left", []),
                   prefix
                       (ctor "blue" [] __LINE__)
                       (prefix (ctor "sync" [] __LINE__) (unwind "Left" [] __LINE__) __LINE__)
                       __LINE__)
                  (("Right", []),
                   prefix
                       (ctor "red" [] __LINE__)
                       (prefix (ctor "sync" [] __LINE__) (unwind "Right" [] __LINE__) __LINE__)
                       __LINE__) ]) in

    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])

    let genv = Env.empty in

    match dot pm cm genv "LR" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={})"
  "(Left ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={})"
  "((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ Right env={})"
  "(Left ⟦(Set.insert sync (event set).empty)⟧ Right env={})"
  "((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={})" -> "(Left ⟦(Set.insert sync (event set).empty)⟧ Right env={})" [label="sync"]
  "(Left ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={})" [label="blue"]
  "((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ Right env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={})" [label="red"]
  "(Left ⟦(Set.insert sync (event set).empty)⟧ Right env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ Right env={})" [label="blue"]
  "(Left ⟦(Set.insert sync (event set).empty)⟧ Right env={})" -> "(Left ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={})" [label="red"]
}""" =
                actual,
            actual
        )

[<Fact>]
let coinToss () =
    let tEvent =
        tUnion "event" [ ("toss", []); ("heads", []); ("tails", []); ("right", []); ("left", []) ] in

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("Coin", []), prefix (ctor "toss" [] __LINE__) (unwind "Coin'" [] __LINE__) __LINE__)
                  (("Coin'", []),
                   intCh
                       (prefix (ctor "heads" [] __LINE__) (unwind "Coin" [] __LINE__) __LINE__)
                       (prefix (ctor "tails" [] __LINE__) (unwind "Coin" [] __LINE__) __LINE__)
                       __LINE__)
                  (("Man", []), prefix (ctor "toss" [] __LINE__) (unwind "Man'" [] __LINE__) __LINE__)
                  (("Man'", []),
                   extCh
                       (prefix
                           (ctor "heads" [] __LINE__)
                           (prefix (ctor "left" [] __LINE__) (unwind "Man" [] __LINE__) __LINE__)
                           __LINE__)
                       (prefix
                           (ctor "tails" [] __LINE__)
                           (prefix (ctor "right" [] __LINE__) (unwind "Man" [] __LINE__) __LINE__)
                           __LINE__)
                       __LINE__)
                  (("CoinToss", []),
                   interfaceParallel
                       (unwind "Coin" [] __LINE__)
                       (setInsert
                           (ctor "toss" [] __LINE__)
                           (setInsert
                               (ctor "heads" [] __LINE__)
                               (setInsert (ctor "tails" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
                               __LINE__)
                           __LINE__)
                       (unwind "Man" [] __LINE__)
                       __LINE__) ])

    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])
    let genv = Env.empty in

    match dot pm cm genv "CoinToss" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ (left -> Man env={}) env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ (right -> Man env={}) env={})"
  "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})"
  "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})"
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ (left -> Man env={}) env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man env={})" [label="left"]
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ (right -> Man env={}) env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man env={})" [label="right"]
  "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ (left -> Man env={}) env={})" [label="heads"]
  "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ (right -> Man env={}) env={})" [label="tails"]
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})" -> "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})" [label="τ"]
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})" -> "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})" [label="τ"]
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man env={})" -> "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails (event set).empty)))⟧ Man' env={})" [label="toss"]
}""" =
                actual,
            actual
        )

[<Fact>]
let lrh () =
    let tEvent = tUnion "event" [ ("blue", []); ("red", []); ("sync", []) ] in

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("LRH", []),
                   hide
                       (interfaceParallel
                           (unwind "Left" [] __LINE__)
                           (setInsert (ctor "sync" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
                           (unwind "Right" [] __LINE__)
                           __LINE__)
                       (setInsert (ctor "sync" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
                       __LINE__)
                  (("Left", []),
                   prefix
                       (ctor "blue" [] __LINE__)
                       (prefix (ctor "sync" [] __LINE__) (unwind "Left" [] __LINE__) __LINE__)
                       __LINE__)
                  (("Right", []),
                   prefix
                       (ctor "red" [] __LINE__)
                       (prefix (ctor "sync" [] __LINE__) (unwind "Right" [] __LINE__) __LINE__)
                       __LINE__) ]) in

    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])
    let genv = Env.empty in

    match dot pm cm genv "LRH" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "(((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync (event set).empty) env={})"
  "((Left ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync (event set).empty) env={})"
  "(((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ Right env={}) \\ (Set.insert sync (event set).empty) env={})"
  "((Left ⟦(Set.insert sync (event set).empty)⟧ Right env={}) \\ (Set.insert sync (event set).empty) env={})"
  "(((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync (event set).empty) env={})" -> "((Left ⟦(Set.insert sync (event set).empty)⟧ Right env={}) \\ (Set.insert sync (event set).empty) env={})" [label="τ (sync)"]
  "((Left ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync (event set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync (event set).empty) env={})" [label="blue"]
  "(((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ Right env={}) \\ (Set.insert sync (event set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync (event set).empty) env={})" [label="red"]
  "((Left ⟦(Set.insert sync (event set).empty)⟧ Right env={}) \\ (Set.insert sync (event set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync (event set).empty)⟧ Right env={}) \\ (Set.insert sync (event set).empty) env={})" [label="blue"]
  "((Left ⟦(Set.insert sync (event set).empty)⟧ Right env={}) \\ (Set.insert sync (event set).empty) env={})" -> "((Left ⟦(Set.insert sync (event set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync (event set).empty) env={})" [label="red"]
}""" =
                actual,
            actual
        )

[<Fact>]
let hide3 () =
    let tEvent = tUnion "event" [ ("a", []) ] in

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ ("P", []),
                  hide
                      (prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__)
                      (setInsert (ctor "a" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
                      __LINE__ ]) in

    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])
    let genv = Env.empty in

    match dot pm cm genv "P" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "Ω"
  "(SKIP \\ (Set.insert a (event set).empty) env={})"
  "((a -> SKIP env={}) \\ (Set.insert a (event set).empty) env={})"
  "(SKIP \\ (Set.insert a (event set).empty) env={})" -> "Ω" [label="✓"]
  "((a -> SKIP env={}) \\ (Set.insert a (event set).empty) env={})" -> "(SKIP \\ (Set.insert a (event set).empty) env={})" [label="τ (a)"]
}""" =
                actual,
            actual
        )

[<Fact>]
let count () =
    let tEvent = tUnion "event" [ ("push", []); ("reset", []) ] in

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("COUNT", [ ("n", tNat) ]),
                   extCh
                       (guard
                           (less tNat (varRef "n" __LINE__) (litNat 10u __LINE__) __LINE__)
                           (prefix
                               (ctor "push" [] __LINE__)
                               (unwind
                                   "COUNT"
                                   [ plus tNat (varRef "n" __LINE__) (litNat 1u __LINE__) __LINE__ ]
                                   __LINE__)
                               __LINE__)
                           __LINE__)
                       (guard
                           (eq tNat (varRef "n" __LINE__) (litNat 10u __LINE__) __LINE__)
                           (prefix (ctor "reset" [] __LINE__) (unwind "COUNT" [ litNat 0u __LINE__ ] __LINE__) __LINE__)
                           __LINE__)
                       __LINE__) ]) in

    let cm = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])
    let genv = Env.empty in

    match dot pm cm genv "COUNT" [ vNat 0u ] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=10} env={n=10}) else STOP) env={n=10}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=10} env={n=10}) else STOP) env={n=10}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=9} env={n=9}) else STOP) env={n=9}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=9} env={n=9}) else STOP) env={n=9}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=8} env={n=8}) else STOP) env={n=8}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=8} env={n=8}) else STOP) env={n=8}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=7} env={n=7}) else STOP) env={n=7}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=7} env={n=7}) else STOP) env={n=7}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=6} env={n=6}) else STOP) env={n=6}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=6} env={n=6}) else STOP) env={n=6}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=5} env={n=5}) else STOP) env={n=5}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=5} env={n=5}) else STOP) env={n=5}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=4} env={n=4}) else STOP) env={n=4}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=4} env={n=4}) else STOP) env={n=4}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=3} env={n=3}) else STOP) env={n=3}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=3} env={n=3}) else STOP) env={n=3}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=2} env={n=2}) else STOP) env={n=2}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=2} env={n=2}) else STOP) env={n=2}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=1} env={n=1}) else STOP) env={n=1}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=1} env={n=1}) else STOP) env={n=1}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=0} env={n=0}) else STOP) env={n=0}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=0} env={n=0}) else STOP) env={n=0}))"
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=10} env={n=10}) else STOP) env={n=10}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=10} env={n=10}) else STOP) env={n=10}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=0} env={n=0}) else STOP) env={n=0}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=0} env={n=0}) else STOP) env={n=0}))" [label="reset"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=9} env={n=9}) else STOP) env={n=9}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=9} env={n=9}) else STOP) env={n=9}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=10} env={n=10}) else STOP) env={n=10}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=10} env={n=10}) else STOP) env={n=10}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=8} env={n=8}) else STOP) env={n=8}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=8} env={n=8}) else STOP) env={n=8}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=9} env={n=9}) else STOP) env={n=9}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=9} env={n=9}) else STOP) env={n=9}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=7} env={n=7}) else STOP) env={n=7}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=7} env={n=7}) else STOP) env={n=7}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=8} env={n=8}) else STOP) env={n=8}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=8} env={n=8}) else STOP) env={n=8}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=6} env={n=6}) else STOP) env={n=6}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=6} env={n=6}) else STOP) env={n=6}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=7} env={n=7}) else STOP) env={n=7}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=7} env={n=7}) else STOP) env={n=7}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=5} env={n=5}) else STOP) env={n=5}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=5} env={n=5}) else STOP) env={n=5}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=6} env={n=6}) else STOP) env={n=6}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=6} env={n=6}) else STOP) env={n=6}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=4} env={n=4}) else STOP) env={n=4}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=4} env={n=4}) else STOP) env={n=4}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=5} env={n=5}) else STOP) env={n=5}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=5} env={n=5}) else STOP) env={n=5}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=3} env={n=3}) else STOP) env={n=3}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=3} env={n=3}) else STOP) env={n=3}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=4} env={n=4}) else STOP) env={n=4}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=4} env={n=4}) else STOP) env={n=4}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=2} env={n=2}) else STOP) env={n=2}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=2} env={n=2}) else STOP) env={n=2}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=3} env={n=3}) else STOP) env={n=3}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=3} env={n=3}) else STOP) env={n=3}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=1} env={n=1}) else STOP) env={n=1}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=1} env={n=1}) else STOP) env={n=1}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=2} env={n=2}) else STOP) env={n=2}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=2} env={n=2}) else STOP) env={n=2}))" [label="push"]
  "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=0} env={n=0}) else STOP) env={n=0}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=0} env={n=0}) else STOP) env={n=0}))" -> "((if (nat.less n 10) then (push -> COUNT (nat.plus n 1) env={n=1} env={n=1}) else STOP) env={n=1}) □ (if (nat.equal n 10) then (reset -> COUNT 0 env={n=1} env={n=1}) else STOP) env={n=1}))" [label="push"]
}""" =
                actual,
            actual
        )

[<Fact>]
let roVarSys1 () =
    let evs =
        setInsert
            (litNat 0u __LINE__)
            (setInsert
                (litNat 1u __LINE__)
                (setInsert
                    (litNat 2u __LINE__)
                    (setInsert
                        (litNat 3u __LINE__)
                        (setInsert
                            (litNat 4u __LINE__)
                            (setInsert (litNat 5u __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__)
                            __LINE__)
                        __LINE__)
                    __LINE__)
                __LINE__)
            __LINE__

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("ROVarSys1", []),
                   interfaceParallel
                       (unwind "ROVar" [ litNat 0u __LINE__ ] __LINE__)
                       evs
                       (interfaceParallel
                           (unwind "Reader1" [] __LINE__)
                           evs
                           (interfaceParallel (unwind "Reader2" [] __LINE__) evs (unwind "Reader3" [] __LINE__) __LINE__)
                           __LINE__)
                       __LINE__)
                  (("ROVar", [ ("n", tNat) ]),
                   prefix
                       (varRef "n" __LINE__)
                       (unwind
                           "ROVar"
                           [ ifExpr
                                 (less tNat (varRef "n" __LINE__) (litNat 4u __LINE__) __LINE__)
                                 (plus tNat (varRef "n" __LINE__) (litNat 1u __LINE__) __LINE__)
                                 (litNat 0u __LINE__)
                                 __LINE__ ]
                           __LINE__)
                       __LINE__)
                  (("Reader1", []), prefixRecv evs "n" (stop __LINE__) __LINE__)
                  (("Reader2", []), prefixRecv evs "n" (stop __LINE__) __LINE__)
                  (("Reader3", []), prefixRecv evs "n" (stop __LINE__) __LINE__) ]) in

    let cm = CtorMap.empty
    let genv = Env.empty in

    match dot pm cm genv "ROVarSys1" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "(ROVar (if (nat.less n 4) then (nat.plus n 1) else 0) env={n=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ STOP env={}) env={}) env={})"  [fillcolor=red, style=filled, fontcolor=white]
  "(ROVar 0 env={} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader2 ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ Reader3 env={}) env={}) env={})"
  "(ROVar 0 env={} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader2 ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less n 4) then (nat.plus n 1) else 0) env={n=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ STOP env={}) env={}) env={})" [label="0"]
}""" =
                actual,
            actual
        )

[<Fact>]
let roVarSys2 () =
    let evs =
        setInsert
            (litNat 0u __LINE__)
            (setInsert
                (litNat 1u __LINE__)
                (setInsert
                    (litNat 2u __LINE__)
                    (setInsert
                        (litNat 3u __LINE__)
                        (setInsert
                            (litNat 4u __LINE__)
                            (setInsert (litNat 5u __LINE__) (litEmpty (tSet tNat) __LINE__) __LINE__)
                            __LINE__)
                        __LINE__)
                    __LINE__)
                __LINE__)
            __LINE__

    let pm =
        ResultEx.get
            ProcMapError.format
            (from
                [ (("ROVarSys2", []),
                   (interfaceParallel
                       (unwind "ROVar" [ litNat 0u __LINE__ ] __LINE__)
                       evs
                       (interleave
                           (unwind "Reader1" [] __LINE__)
                           (interleave (unwind "Reader2" [] __LINE__) (unwind "Reader3" [] __LINE__) __LINE__)
                           __LINE__)
                       __LINE__))
                  (("ROVar", [ ("x", tNat) ]),
                   (prefix
                       (varRef "x" __LINE__)
                       (unwind
                           "ROVar"
                           [ ifExpr
                                 (less tNat (varRef "x" __LINE__) (litNat 4u __LINE__) __LINE__)
                                 (plus tNat (varRef "x" __LINE__) (litNat 1u __LINE__) __LINE__)
                                 (litNat 0u __LINE__)
                                 __LINE__ ]
                           __LINE__)
                       __LINE__))
                  (("Reader1", []), prefixRecv evs "x" (stop __LINE__) __LINE__)
                  (("Reader2", []), prefixRecv evs "x" (stop __LINE__) __LINE__)
                  (("Reader3", []), prefixRecv evs "x" (stop __LINE__) __LINE__) ]) in

    let cm = CtorMap.empty
    let genv = Env.empty in

    match dot pm cm genv "ROVarSys2" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ STOP env={}) env={}) env={})"  [fillcolor=red, style=filled, fontcolor=white]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})"
  "(ROVar 0 env={} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ STOP env={}) env={}) env={})" [label="2"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ STOP env={}) env={}) env={})" [label="2"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ STOP env={}) env={}) env={})" [label="2"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" [label="1"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ STOP env={}) env={}) env={})" [label="1"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ STOP env={}) env={}) env={})" [label="1"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ STOP env={}) env={}) env={})" [label="1"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ STOP env={}) env={}) env={})" [label="1"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" [label="1"]
  "(ROVar 0 env={} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (STOP ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" [label="0"]
  "(ROVar 0 env={} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ STOP env={}) env={}) env={})" [label="0"]
  "(ROVar 0 env={} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (Reader1 ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(Set.insert 0 (Set.insert 1 (Set.insert 2 (Set.insert 3 (Set.insert 4 (Set.insert 5 (nat set).empty))))))⟧ (STOP ⟦(() set).empty⟧ (Reader2 ⟦(() set).empty⟧ Reader3 env={}) env={}) env={})" [label="0"]
}""" =
                actual,
            actual
        )
