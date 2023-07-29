module CSP.Core.Tests.GraphTests

open Xunit
open CSP.Core
open CSP.Core.Search
open CSP.Core.TypeShorthand
open CSP.Core.ExprShorthand
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.Eval
open CSP.Core.DotLang

let evalCfg: EvalConfig = { UnivConfig = { NatMax = 5u; ListLenMax = 3u } }
let dotCfg = dotConfig (searchConfig 100) evalCfg
let dot = dot dotCfg

[<Fact>]
let abSkip () =
    let tEvent = tUnion "event" [ ("a", []); ("b", []) ]

    let pm =
        from
            [ ("ABSkip", []), seq (unwind "ASkip" []) (unwind "BSkip" [])
              ("ASkip", []), prefix (ctor "a" []) skip
              ("BSkip", []), prefix (ctor "b" []) skip ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "ABSkip" [] in

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
    let tEvent = tUnion "event" [ ("a", []); ("b", []); ("c", []); ("d", []) ]

    let pm =
        from
            [ (("ParABC", []),
               interleave
                   (prefix (ctor "a" []) skip)
                   (interleave (prefix (ctor "b" []) skip) (prefix (ctor "c" []) skip)))
              (("P", []), seq (unwind "ParABC" []) (prefix (ctor "d" []) skip)) ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "P" [] in

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
        from [ (("P", []), intCh (prefix (litNat 1u) (unwind "P" [])) (prefix (litNat 2u) (unwind "P" []))) ] in

    let cm = CtorMap.empty
    let genv = Env.empty in
    let actual = dot pm cm genv "P" [] in

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
    let tEvent = tUnion "event" [ ("a", []); ("b", []); ("s", []) ]

    let pm =
        from
            [ (("ABS", []),
               (extCh
                   (intCh (prefix (ctor "a" []) (unwind "ABS" [])) (prefix (ctor "b" []) (unwind "ABS" [])))
                   (prefix (ctor "s" []) stop))) ] in

    let cm = CtorMap.from [ tEvent ]
    let genv = Env.empty in
    let actual = dot pm cm genv "ABS" [] in

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
    let tEvent = tUnion "event" [ ("blue", []); ("red", []); ("sync", []) ]

    let pm =
        from
            [ (("LR", []),
               (interfaceParallel
                   (unwind "Left" [])
                   (setInsert (ctor "sync" []) (litEmpty (tSet tEvent)))
                   (unwind "Right" [])))
              (("Left", []), prefix (ctor "blue" []) (prefix (ctor "sync" []) (unwind "Left" [])))
              (("Right", []), prefix (ctor "red" []) (prefix (ctor "sync" []) (unwind "Right" []))) ] in

    let cm = CtorMap.from [ tEvent ] in

    let genv = Env.empty in
    let actual = dot pm cm genv "LR" [] in

    Assert.True(
        """digraph G {
  "((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={})"
  "(Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={})"
  "((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={})"
  "(Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={})"
  "((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={})" -> "(Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={})" [label="sync"]
  "(Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={})" [label="blue"]
  "((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={})" [label="red"]
  "(Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={})" [label="blue"]
  "(Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={})" -> "(Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={})" [label="red"]
}""" =
            actual,
        actual
    )

[<Fact>]
let coinToss () =
    let tEvent =
        tUnion "event" [ ("toss", []); ("heads", []); ("tails", []); ("right", []); ("left", []) ]

    let pm =
        from
            [ (("Coin", []), prefix (ctor "toss" []) (unwind "Coin'" []))
              (("Coin'", []),
               intCh (prefix (ctor "heads" []) (unwind "Coin" [])) (prefix (ctor "tails" []) (unwind "Coin" [])))
              (("Man", []), prefix (ctor "toss" []) (unwind "Man'" []))
              (("Man'", []),
               extCh
                   (prefix (ctor "heads" []) (prefix (ctor "left" []) (unwind "Man" [])))
                   (prefix (ctor "tails" []) (prefix (ctor "right" []) (unwind "Man" []))))
              (("CoinToss", []),
               interfaceParallel
                   (unwind "Coin" [])
                   (setInsert
                       (ctor "toss" [])
                       (setInsert (ctor "heads" []) (setInsert (ctor "tails" []) (litEmpty (tSet tEvent)))))
                   (unwind "Man" [])) ]

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "CoinToss" [] in

    Assert.True(
        """digraph G {
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ (left -> Man env={}) env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ (right -> Man env={}) env={})"
  "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})"
  "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})"
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ (left -> Man env={}) env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man env={})" [label="left"]
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ (right -> Man env={}) env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man env={})" [label="right"]
  "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ (left -> Man env={}) env={})" [label="heads"]
  "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ (right -> Man env={}) env={})" [label="tails"]
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})" -> "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})" [label="τ"]
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})" -> "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})" [label="τ"]
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man env={})" -> "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((event heads []/left []/right []/tails []/toss []) set).empty)))⟧ Man' env={})" [label="toss"]
}""" =
            actual,
        actual
    )

[<Fact>]
let lrh () =
    let tEvent = tUnion "event" [ ("blue", []); ("red", []); ("sync", []) ]

    let pm =
        from
            [ (("LRH", []),
               hide
                   (interfaceParallel
                       (unwind "Left" [])
                       (setInsert (ctor "sync" []) (litEmpty (tSet tEvent)))
                       (unwind "Right" []))
                   (setInsert (ctor "sync" []) (litEmpty (tSet tEvent))))
              (("Left", []), prefix (ctor "blue" []) (prefix (ctor "sync" []) (unwind "Left" [])))
              (("Right", []), prefix (ctor "red" []) (prefix (ctor "sync" []) (unwind "Right" []))) ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "LRH" [] in

    Assert.True(
        """digraph G {
  "(((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})"
  "((Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})"
  "(((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})"
  "((Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})"
  "(((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" -> "((Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" [label="τ (sync)"]
  "((Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" [label="blue"]
  "(((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" [label="red"]
  "((Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" [label="blue"]
  "((Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ Right env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" -> "((Left ⟦(Set.insert sync ((event blue []/red []/sync []) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((event blue []/red []/sync []) set).empty) env={})" [label="red"]
}""" =
            actual,
        actual
    )

[<Fact>]
let hide3 () =
    let tEvent = tUnion "event" [ ("a", []) ] in

    let pm =
        from [ ("P", []), hide (prefix (ctor "a" []) skip) (setInsert (ctor "a" []) (litEmpty (tSet tEvent))) ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "P" [] in

    Assert.True(
        """digraph G {
  "Ω"
  "(SKIP \\ (Set.insert a ((event a []) set).empty) env={})"
  "((a -> SKIP env={}) \\ (Set.insert a ((event a []) set).empty) env={})"
  "(SKIP \\ (Set.insert a ((event a []) set).empty) env={})" -> "Ω" [label="✓"]
  "((a -> SKIP env={}) \\ (Set.insert a ((event a []) set).empty) env={})" -> "(SKIP \\ (Set.insert a ((event a []) set).empty) env={})" [label="τ (a)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let count () =
    let tEvent = tUnion "event" [ ("push", []); ("reset", []) ] in

    let pm =
        from
            [ (("COUNT", [ "n" ]),
               extCh
                   (guard
                       (less tNat (varRef "n") (litNat 10u))
                       (prefix (ctor "push" []) (unwind "COUNT" [ plus tNat (varRef "n") (litNat 1u) ])))
                   (guard (eq tNat (varRef "n") (litNat 10u)) (prefix (ctor "reset" []) (unwind "COUNT" [ litNat 0u ])))) ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "COUNT" [ litNat 0u ] in

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
            (litNat 0u)
            (setInsert
                (litNat 1u)
                (setInsert
                    (litNat 2u)
                    (setInsert (litNat 3u) (setInsert (litNat 4u) (setInsert (litNat 5u) (litEmpty (tSet tNat)))))))

    let pm =
        from
            [ (("ROVarSys1", []),
               interfaceParallel
                   (unwind "ROVar" [ litNat 0u ])
                   evs
                   (interfaceParallel
                       (unwind "Reader1" [])
                       evs
                       (interfaceParallel (unwind "Reader2" []) evs (unwind "Reader3" []))))
              (("ROVar", [ "n" ]),
               prefix
                   (varRef "n")
                   (unwind
                       "ROVar"
                       [ ifExpr (less tNat (varRef "n") (litNat 4u)) (plus tNat (varRef "n") (litNat 1u)) (litNat 0u) ]))
              (("Reader1", []), prefixRecv evs "n" stop)
              (("Reader2", []), prefixRecv evs "n" stop)
              (("Reader3", []), prefixRecv evs "n" stop) ] in

    let cm = CtorMap.empty
    let genv = Env.empty in
    let actual = dot pm cm genv "ROVarSys1" [] in

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
            (litNat 0u)
            (setInsert
                (litNat 1u)
                (setInsert
                    (litNat 2u)
                    (setInsert (litNat 3u) (setInsert (litNat 4u) (setInsert (litNat 5u) (litEmpty (tSet tNat)))))))

    let pm =
        from
            [ (("ROVarSys2", []),
               (interfaceParallel
                   (unwind "ROVar" [ litNat 0u ])
                   evs
                   (interleave (unwind "Reader1" []) (interleave (unwind "Reader2" []) (unwind "Reader3" [])))))
              (("ROVar", [ "x" ]),
               (prefix
                   (varRef "x")
                   (unwind
                       "ROVar"
                       [ ifExpr (less tNat (varRef "x") (litNat 4u)) (plus tNat (varRef "x") (litNat 1u)) (litNat 0u) ])))
              (("Reader1", []), prefixRecv evs "x" stop)
              (("Reader2", []), prefixRecv evs "x" stop)
              (("Reader3", []), prefixRecv evs "x" stop) ] in

    let cm = CtorMap.empty
    let genv = Env.empty in
    let actual = dot pm cm genv "ROVarSys2" [] in

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
