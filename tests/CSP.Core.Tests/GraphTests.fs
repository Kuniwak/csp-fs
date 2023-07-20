module CSP.Core.Tests.GraphTests

open CSP.Core
open Xunit
open CSP.Core.Ctor
open CSP.Core.Type
open CSP.Core.Var
open CSP.Core.Val
open CSP.Core.ExprShorthand
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.Graph

let max = 100

[<Fact>]
let abSkip () =
    let pm =
        from
            [ ("ABSkip", None), seq (unwind "ASkip" None) (unwind "BSkip" None)
              ("ASkip", None), prefix (ctor "a" litUnit) skip
              ("BSkip", None), prefix (ctor "b" litUnit) skip ] in

    let cm = CtorMap.empty in
    let genv = Env.empty in
    let actual = dot max pm cm genv "ABSkip" None in

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
    let pm =
        from
            [ (("ParABC", None),
               interleave
                   (prefix (ctor "a" litUnit) skip)
                   (interleave (prefix (ctor "b" litUnit) skip) (prefix (ctor "c" litUnit) skip)))
              (("P", None), seq (unwind "ParABC" None) (prefix (ctor "d" litUnit) skip)) ] in

    let cm = CtorMap.empty in
    let genv = Env.empty in
    let actual = dot max pm cm genv "P" None in

    Assert.True(
        """digraph G {
  "Ω"
  "SKIP"
  "(d -> SKIP env={})"
  "((Ω ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(ParABC ; (d -> SKIP env={}))"
  "SKIP" -> "Ω" [label="✓"]
  "(d -> SKIP env={})" -> "SKIP" [label="d"]
  "((Ω ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" -> "(d -> SKIP env={})" [label="τ"]
  "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (Ω ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(ParABC ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ (SKIP ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(ParABC ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(ParABC ; (d -> SKIP env={}))" -> "((SKIP ⟦(unit set).empty⟧ ((b -> SKIP env={}) ⟦(unit set).empty⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
}""" =
            actual,
        actual
    )

[<Fact>]
let rand2 () =
    let pm =
        from [ (("P", None), intCh (prefix (litNat 1u) (unwind "P" None)) (prefix (litNat 2u) (unwind "P" None))) ] in

    let cm = CtorMap.empty
    let genv = Env.empty in
    let actual = dot max pm cm genv "P" None in

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
    let pm =
        from
            [ (("ABS", None),
               (extCh
                   (intCh
                       (prefix (ctor "a" litUnit) (unwind "ABS" None))
                       (prefix (ctor "b" litUnit) (unwind "ABS" None)))
                   (prefix (ctor "s" litUnit) stop))) ] in

    let cm = CtorMap.empty
    let genv = Env.empty in
    let actual = dot max pm cm genv "ABS" None in

    Assert.True(
        """digraph G {
  "STOP"  [fillcolor=red, style=filled, fontcolor=white]
  "((a -> ABS env={}) □ (s -> STOP env={}))"
  "((b -> ABS env={}) □ (s -> STOP env={}))"
  "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))"
  "((a -> ABS env={}) □ (s -> STOP env={}))" -> "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" [label="a"]
  "((a -> ABS env={}) □ (s -> STOP env={}))" -> "STOP" [label="s"]
  "((b -> ABS env={}) □ (s -> STOP env={}))" -> "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" [label="b"]
  "((b -> ABS env={}) □ (s -> STOP env={}))" -> "STOP" [label="s"]
  "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" -> "((b -> ABS env={}) □ (s -> STOP env={}))" [label="τ"]
  "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" -> "((a -> ABS env={}) □ (s -> STOP env={}))" [label="τ"]
  "(((a -> ABS env={}) ⨅ (b -> ABS env={})) □ (s -> STOP env={}))" -> "STOP" [label="s"]
}""" =
            actual,
        actual
    )

[<Fact>]
let lr () =
    let syncEvent = ctor "sync" litUnit in

    let pm =
        from
            [ (("LR", None),
               (interfaceParallel
                   (unwind "Left" None)
                   (setInsert syncEvent (litEmpty (TSet(TUnion("event", TUnit)))))
                   (unwind "Right" None)))
              (("Left", None), prefix (ctor "blue" litUnit) (prefix syncEvent (unwind "Left" None)))
              (("Right", None), prefix (ctor "red" litUnit) (prefix syncEvent (unwind "Right" None))) ] in

    let cm =
        CtorMap.from
            [ ("event", Ctor "blue", TUnit)
              ("event", Ctor "red", TUnit)
              ("event", Ctor "red", TUnit) ] in

    let genv = Env.empty in
    let actual = dot max pm cm genv "LR" None in

    Assert.True(
        """digraph G {
  "((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={})"
  "(Left ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={})"
  "((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={})"
  "(Left ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={})"
  "((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={})" -> "(Left ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={})" [label="sync"]
  "(Left ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={})" [label="blue"]
  "((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={})" [label="red"]
  "(Left ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={})" -> "((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={})" [label="blue"]
  "(Left ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={})" -> "(Left ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={})" [label="red"]
}""" =
            actual,
        actual
    )

[<Fact>]
let coinToss () =
    let pm =
        from
            [ (("Coin", None), prefix (ctor "toss" litUnit) (unwind "Coin'" None))
              (("Coin'", None),
               intCh
                   (prefix (ctor "heads" litUnit) (unwind "Coin" None))
                   (prefix (ctor "tails" litUnit) (unwind "Coin" None)))
              (("Man", None), prefix (ctor "toss" litUnit) (unwind "Man'" None))
              (("Man'", None),
               extCh
                   (prefix (ctor "heads" litUnit) (prefix (ctor "left" litUnit) (unwind "Man" None)))
                   (prefix (ctor "tails" litUnit) (prefix (ctor "right" litUnit) (unwind "Man" None))))
              (("CoinToss", None),
               interfaceParallel
                   (unwind "Coin" None)
                   (setInsert
                       (ctor "toss" litUnit)
                       (setInsert (ctor "heads" litUnit) (setInsert (ctor "tails" litUnit) (litEmpty (TSet (TUnion("event", TUnit)))))))
                   (unwind "Man" None)) ]

    let cm =
        CtorMap.from
            [ ("event", Ctor "toss", TUnit)
              ("event", Ctor "heads", TUnit)
              ("event", Ctor "tails", TUnit)
              ("event", Ctor "left", TUnit)
              ("event", Ctor "right", TUnit) ] in

    let genv = Env.empty in
    let actual = dot max pm cm genv "CoinToss" None in

    Assert.True(
        """digraph G {
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ (left -> Man env={}) env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ (right -> Man env={}) env={})"
  "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})"
  "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})"
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man env={})"
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ (left -> Man env={}) env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man env={})" [label="left"]
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ (right -> Man env={}) env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man env={})" [label="right"]
  "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ (left -> Man env={}) env={})" [label="heads"]
  "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})" -> "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ (right -> Man env={}) env={})" [label="tails"]
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})" -> "((tails -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})" [label="τ"]
  "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})" -> "((heads -> Coin env={}) ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})" [label="τ"]
  "(Coin ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man env={})" -> "(Coin' ⟦(Set.insert toss (Set.insert heads (Set.insert tails ((unit event) set).empty)))⟧ Man' env={})" [label="toss"]
}""" =
            actual,
        actual
    )

[<Fact>]
let lrh () =
    let pm =
        from
            [ (("LRH", None),
               hide
                   (interfaceParallel
                       (unwind "Left" None)
                       (setInsert (ctor "sync" litUnit) (litEmpty (TSet(TUnion("event", TUnit)))))
                       (unwind "Right" None))
                   (setInsert (ctor "sync" litUnit) (litEmpty (TSet (TUnion("event", TUnit))))))
              (("Left", None), prefix (ctor "blue" litUnit) (prefix (ctor "sync" litUnit) (unwind "Left" None)))
              (("Right", None), prefix (ctor "red" litUnit) (prefix (ctor "sync" litUnit) (unwind "Right" None))) ] in

    let cm =
        CtorMap.from
            [ (UnionName "event", Ctor "blue", TUnit)
              (UnionName "event", Ctor "red", TUnit)
              (UnionName "event", Ctor "sync", TUnit) ] in

    let genv = Env.empty in
    let actual = dot max pm cm genv "LRH" None in

    Assert.True(
        """digraph G {
  "(((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((unit event) set).empty) env={})"
  "((Left ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((unit event) set).empty) env={})"
  "(((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={}) \\ (Set.insert sync ((unit event) set).empty) env={})"
  "((Left ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={}) \\ (Set.insert sync ((unit event) set).empty) env={})"
  "(((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" -> "((Left ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" [label="τ (sync)"]
  "((Left ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" [label="blue"]
  "(((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" [label="red"]
  "((Left ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" -> "(((sync -> Left env={}) ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" [label="blue"]
  "((Left ⟦(Set.insert sync ((unit event) set).empty)⟧ Right env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" -> "((Left ⟦(Set.insert sync ((unit event) set).empty)⟧ (sync -> Right env={}) env={}) \\ (Set.insert sync ((unit event) set).empty) env={})" [label="red"]
}""" =
            actual,
        actual
    )

[<Fact>]
let hide3 () =
    let pm =
        from
            [ ("P", None),
              hide
                  (prefix (ctor "a" litUnit) skip)
                  (setInsert (ctor "a" litUnit) (litEmpty (TSet(TUnion("event", TUnit))))) ] in

    let cm = CtorMap.from [ ("event", Ctor "a", TUnit) ] in
    let genv = Env.empty in
    let actual = dot max pm cm genv "P" None in

    Assert.True(
        """digraph G {
  "Ω"
  "(SKIP \\ (Set.insert a ((unit event) set).empty) env={})"
  "((a -> SKIP env={}) \\ (Set.insert a ((unit event) set).empty) env={})"
  "(SKIP \\ (Set.insert a ((unit event) set).empty) env={})" -> "Ω" [label="✓"]
  "((a -> SKIP env={}) \\ (Set.insert a ((unit event) set).empty) env={})" -> "(SKIP \\ (Set.insert a ((unit event) set).empty) env={})" [label="τ (a)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let count () =
    let pm =
        from
            [ (("COUNT", Some(Var "n")),
               extCh
                   (guard
                       (less TNat (varRef "n") (litNat 10u))
                       (prefix (ctor "push" litUnit) (unwind "COUNT" (Some(plus TNat (varRef "n") (litNat 1u))))))
                   (guard
                       (eq TNat (varRef "n") (litNat 10u))
                       (prefix (ctor "reset" litUnit) (unwind "COUNT" (Some(litNat 0u)))))) ] in

    let cm = CtorMap.empty in
    let genv = Env.empty in
    let actual = dot max pm cm genv "COUNT" (Some(VNat 0u)) in

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
    let readEvs = univ (TUnion("read", TUnit))

    let pm =
        from
            [ (("ROVarSys1", None),
               interfaceParallel
                   (unwind "ROVar" (Some(litNat 0u)))
                   readEvs
                   (interfaceParallel
                       (unwind "Reader1" None)
                       readEvs
                       (interfaceParallel (unwind "Reader2" None) readEvs (unwind "Reader3" None))))
              (("ROVar", Some(Var "x")),
               prefix
                   (ctor "Read" (varRef "x"))
                   (unwind
                       "ROVar"
                       (Some(
                           ifExpr
                               (less TNat (varRef "x") (litNat 4u))
                               (plus TNat (varRef "x") (litNat 1u))
                               (litNat 0u)
                       ))))
              (("Reader1", None), prefixRecv readEvs "x" stop)
              (("Reader2", None), prefixRecv readEvs "x" stop)
              (("Reader3", None), prefixRecv readEvs "x" stop) ] in

    let cm = Map [ (Ctor "Read", ("read", TNat)) ]
    let genv = Env.empty in
    let actual = dot max pm cm genv "ROVarSys1" None in

    Assert.True(
        """digraph G {
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (STOP ⟦(univ::(unit read))⟧ (STOP ⟦(univ::(unit read))⟧ STOP env={}) env={}) env={})"  [fillcolor=red, style=filled, fontcolor=white]
  "(ROVar 0 env={} ⟦(univ::(unit read))⟧ (Reader1 ⟦(univ::(unit read))⟧ (Reader2 ⟦(univ::(unit read))⟧ Reader3 env={}) env={}) env={})"
  "(ROVar 0 env={} ⟦(univ::(unit read))⟧ (Reader1 ⟦(univ::(unit read))⟧ (Reader2 ⟦(univ::(unit read))⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (STOP ⟦(univ::(unit read))⟧ (STOP ⟦(univ::(unit read))⟧ STOP env={}) env={}) env={})" [label="(Read 0)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let roVarSys2 () =
    let readEvs = univ (TUnion("read", TUnit)) in

    let pm =
        from
            [ (("ROVarSys2", None),
               (interfaceParallel
                   (unwind "ROVar" (Some(litNat 0u)))
                   readEvs
                   (interleave (unwind "Reader1" None) (interleave (unwind "Reader2" None) (unwind "Reader3" None)))))
              (("ROVar", Some(Var "x")),
               (prefix
                   (ctor "Read" (varRef "x"))
                   (unwind
                       "ROVar"
                       (Some(
                           ifExpr (less TNat (varRef "x") (litNat 4u)) (plus TNat (varRef "x") (litNat 1u)) (litNat 0u)
                       )))))
              (("Reader1", None), prefixRecv readEvs "x" stop)
              (("Reader2", None), prefixRecv readEvs "x" stop)
              (("Reader3", None), prefixRecv readEvs "x" stop) ] in

    let cm = Map [ (Ctor "Read", ("read", TNat)) ]

    let genv = Env.empty in
    let actual = dot max pm cm genv "ROVarSys2" None in

    Assert.True(
        """digraph G {
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ STOP env={}) env={}) env={})"  [fillcolor=red, style=filled, fontcolor=white]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})"
  "(ROVar 0 env={} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" [label="(Read 2)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" [label="(Read 2)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" [label="(Read 2)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar 0 env={} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (STOP ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" [label="(Read 0)"]
  "(ROVar 0 env={} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ STOP env={}) env={}) env={})" [label="(Read 0)"]
  "(ROVar 0 env={} ⟦(univ::(unit read))⟧ (Reader1 ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(unit read))⟧ (STOP ⟦(unit set).empty⟧ (Reader2 ⟦(unit set).empty⟧ Reader3 env={}) env={}) env={})" [label="(Read 0)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let testMax () =
    let pm =
        from
            [ (("P", Some(Var "n")),
               extCh
                   (prefix (ctor "inc" litUnit) (unwind "P" (Some(plus TNat (varRef "n") (litNat 1u)))))
                   (prefix (ctor "dec" litUnit) (unwind "P" (Some(minus TNat (varRef "n") (litNat 1u)))))) ] in

    let cm = CtorMap.from [ ("event", Ctor "inc", TUnit); ("event", Ctor "dec", TUnit) ]
    let genv = Env.empty in
    let actual = dot 10 pm cm genv "P" (Some(VNat 0u)) in

    Assert.True(
        """digraph G {
  "((inc -> P (nat.plus n 1) env={n=9} env={n=9}) □ (dec -> P (nat.minus n 1) env={n=9} env={n=9}))"
  "((inc -> P (nat.plus n 1) env={n=8} env={n=8}) □ (dec -> P (nat.minus n 1) env={n=8} env={n=8}))"
  "((inc -> P (nat.plus n 1) env={n=7} env={n=7}) □ (dec -> P (nat.minus n 1) env={n=7} env={n=7}))"
  "((inc -> P (nat.plus n 1) env={n=6} env={n=6}) □ (dec -> P (nat.minus n 1) env={n=6} env={n=6}))"
  "((inc -> P (nat.plus n 1) env={n=5} env={n=5}) □ (dec -> P (nat.minus n 1) env={n=5} env={n=5}))"
  "((inc -> P (nat.plus n 1) env={n=4} env={n=4}) □ (dec -> P (nat.minus n 1) env={n=4} env={n=4}))"
  "((inc -> P (nat.plus n 1) env={n=3} env={n=3}) □ (dec -> P (nat.minus n 1) env={n=3} env={n=3}))"
  "((inc -> P (nat.plus n 1) env={n=2} env={n=2}) □ (dec -> P (nat.minus n 1) env={n=2} env={n=2}))"
  "((inc -> P (nat.plus n 1) env={n=1} env={n=1}) □ (dec -> P (nat.minus n 1) env={n=1} env={n=1}))"
  "((inc -> P (nat.plus n 1) env={n=0} env={n=0}) □ (dec -> P (nat.minus n 1) env={n=0} env={n=0}))"
  "((inc -> P (nat.plus n 1) env={n=9} env={n=9}) □ (dec -> P (nat.minus n 1) env={n=9} env={n=9}))" -> "((inc -> P (nat.plus n 1) env={n=10} env={n=10}) □ (dec -> P (nat.minus n 1) env={n=10} env={n=10}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=9} env={n=9}) □ (dec -> P (nat.minus n 1) env={n=9} env={n=9}))" -> "((inc -> P (nat.plus n 1) env={n=8} env={n=8}) □ (dec -> P (nat.minus n 1) env={n=8} env={n=8}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=8} env={n=8}) □ (dec -> P (nat.minus n 1) env={n=8} env={n=8}))" -> "((inc -> P (nat.plus n 1) env={n=9} env={n=9}) □ (dec -> P (nat.minus n 1) env={n=9} env={n=9}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=8} env={n=8}) □ (dec -> P (nat.minus n 1) env={n=8} env={n=8}))" -> "((inc -> P (nat.plus n 1) env={n=7} env={n=7}) □ (dec -> P (nat.minus n 1) env={n=7} env={n=7}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=7} env={n=7}) □ (dec -> P (nat.minus n 1) env={n=7} env={n=7}))" -> "((inc -> P (nat.plus n 1) env={n=8} env={n=8}) □ (dec -> P (nat.minus n 1) env={n=8} env={n=8}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=7} env={n=7}) □ (dec -> P (nat.minus n 1) env={n=7} env={n=7}))" -> "((inc -> P (nat.plus n 1) env={n=6} env={n=6}) □ (dec -> P (nat.minus n 1) env={n=6} env={n=6}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=6} env={n=6}) □ (dec -> P (nat.minus n 1) env={n=6} env={n=6}))" -> "((inc -> P (nat.plus n 1) env={n=7} env={n=7}) □ (dec -> P (nat.minus n 1) env={n=7} env={n=7}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=6} env={n=6}) □ (dec -> P (nat.minus n 1) env={n=6} env={n=6}))" -> "((inc -> P (nat.plus n 1) env={n=5} env={n=5}) □ (dec -> P (nat.minus n 1) env={n=5} env={n=5}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=5} env={n=5}) □ (dec -> P (nat.minus n 1) env={n=5} env={n=5}))" -> "((inc -> P (nat.plus n 1) env={n=6} env={n=6}) □ (dec -> P (nat.minus n 1) env={n=6} env={n=6}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=5} env={n=5}) □ (dec -> P (nat.minus n 1) env={n=5} env={n=5}))" -> "((inc -> P (nat.plus n 1) env={n=4} env={n=4}) □ (dec -> P (nat.minus n 1) env={n=4} env={n=4}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=4} env={n=4}) □ (dec -> P (nat.minus n 1) env={n=4} env={n=4}))" -> "((inc -> P (nat.plus n 1) env={n=5} env={n=5}) □ (dec -> P (nat.minus n 1) env={n=5} env={n=5}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=4} env={n=4}) □ (dec -> P (nat.minus n 1) env={n=4} env={n=4}))" -> "((inc -> P (nat.plus n 1) env={n=3} env={n=3}) □ (dec -> P (nat.minus n 1) env={n=3} env={n=3}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=3} env={n=3}) □ (dec -> P (nat.minus n 1) env={n=3} env={n=3}))" -> "((inc -> P (nat.plus n 1) env={n=4} env={n=4}) □ (dec -> P (nat.minus n 1) env={n=4} env={n=4}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=3} env={n=3}) □ (dec -> P (nat.minus n 1) env={n=3} env={n=3}))" -> "((inc -> P (nat.plus n 1) env={n=2} env={n=2}) □ (dec -> P (nat.minus n 1) env={n=2} env={n=2}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=2} env={n=2}) □ (dec -> P (nat.minus n 1) env={n=2} env={n=2}))" -> "((inc -> P (nat.plus n 1) env={n=3} env={n=3}) □ (dec -> P (nat.minus n 1) env={n=3} env={n=3}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=2} env={n=2}) □ (dec -> P (nat.minus n 1) env={n=2} env={n=2}))" -> "((inc -> P (nat.plus n 1) env={n=1} env={n=1}) □ (dec -> P (nat.minus n 1) env={n=1} env={n=1}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=1} env={n=1}) □ (dec -> P (nat.minus n 1) env={n=1} env={n=1}))" -> "((inc -> P (nat.plus n 1) env={n=2} env={n=2}) □ (dec -> P (nat.minus n 1) env={n=2} env={n=2}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=1} env={n=1}) □ (dec -> P (nat.minus n 1) env={n=1} env={n=1}))" -> "((inc -> P (nat.plus n 1) env={n=0} env={n=0}) □ (dec -> P (nat.minus n 1) env={n=0} env={n=0}))" [label="dec"]
  "((inc -> P (nat.plus n 1) env={n=0} env={n=0}) □ (dec -> P (nat.minus n 1) env={n=0} env={n=0}))" -> "((inc -> P (nat.plus n 1) env={n=1} env={n=1}) □ (dec -> P (nat.minus n 1) env={n=1} env={n=1}))" [label="inc"]
  "((inc -> P (nat.plus n 1) env={n=0} env={n=0}) □ (dec -> P (nat.minus n 1) env={n=0} env={n=0}))" -> "((inc -> P (nat.plus n 1) env={n=0} env={n=0}) □ (dec -> P (nat.minus n 1) env={n=0} env={n=0}))" [label="dec"]
}""" =
            actual,
        actual
    )
