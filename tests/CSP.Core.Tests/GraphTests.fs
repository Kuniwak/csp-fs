module CSP.Core.Tests.GraphTests

open CSP.Core.Type
open Xunit
open CSP.Core.Val
open CSP.Core.Expr
open CSP.Core.ProcMap
open CSP.Core.Proc
open CSP.Core.Graph

let max = 100

[<Fact>]
let abSkip () =
    let m: ProcMap<string, Unit, string> =
        Map
            [ ("ABSkip", (None, Seq(Unwind("ASkip", None), Unwind("BSkip", None))))
              ("ASkip", (None, Prefix(Lit(VUnion(Ctor "a", VUnit)), Skip)))
              ("BSkip", (None, Prefix(Lit(VUnion(Ctor "b", VUnit)), Skip))) ] in

    let env = Map.empty in
    let actual = dot max m env "ABSkip" None in

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
    let m: ProcMap<string, Unit, string> =
        Map
            [ ("ParABC",
               (None,
                Interleave(
                    Prefix(Union(Ctor "a", Lit VUnit), Skip),
                    Interleave(Prefix(Union(Ctor "b", Lit VUnit), Skip), Prefix(Union(Ctor "c", Lit VUnit), Skip))
                )))
              ("P", (None, Seq(Unwind("ParABC", None), Prefix(Union(Ctor "d", Lit VUnit), Skip)))) ] in

    let env = Map.empty in
    let actual = dot max m env "P" None in

    Assert.True(
        """digraph G {
  "Ω"
  "SKIP"
  "(d -> SKIP env={})"
  "((Ω ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))"
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))"
  "(ParABC ; (d -> SKIP env={}))"
  "SKIP" -> "Ω" [label="✓"]
  "(d -> SKIP env={})" -> "SKIP" [label="d"]
  "((Ω ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))" -> "(d -> SKIP env={})" [label="τ"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ Ω env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((Ω ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ Ω env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="τ"]
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
  "(ParABC ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="b"]
  "(ParABC ; (d -> SKIP env={}))" -> "(((a -> SKIP env={}) ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ SKIP env={}) env={}) ; (d -> SKIP env={}))" [label="c"]
  "(ParABC ; (d -> SKIP env={}))" -> "((SKIP ⟦{}⟧ ((b -> SKIP env={}) ⟦{}⟧ (c -> SKIP env={}) env={}) env={}) ; (d -> SKIP env={}))" [label="a"]
}""" =
            actual,
        actual
    )

[<Fact>]
let rand2 () =
    let m: ProcMap<string, int, string> =
        Map [ ("P", (None, IntCh(Prefix(Lit(VNat 1u), Unwind("P", None)), Prefix(Lit(VNat 2u), Unwind("P", None))))) ] in

    let env = Map.empty in
    let actual = dot max m env "P" None in

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
    let m: ProcMap<string, Unit, string> =
        Map
            [ ("ABS",
               (None,
                ExtCh(
                    IntCh(
                        Prefix(Union(Ctor "a", Lit VUnit), Unwind("ABS", None)),
                        Prefix(Union(Ctor "b", Lit VUnit), Unwind("ABS", None))
                    ),
                    Prefix(Union(Ctor "s", Lit VUnit), Stop)
                ))) ] in

    let env = Map.empty in
    let actual = dot max m env "ABS" None in

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
    let m: ProcMap<string, Unit, string> =
        Map
            [ ("LR",
               (None,
                InterfaceParallel(
                    Unwind("Left", None),
                    SetInsert(Union(Ctor "sync", Lit VUnit), SetEmpty),
                    Unwind("Right", None)
                )))
              ("Left",
               (None, Prefix(Union(Ctor "blue", Lit VUnit), Prefix(Union(Ctor "sync", Lit VUnit), Unwind("Left", None)))))
              ("Right",
               (None, Prefix(Union(Ctor "red", Lit VUnit), Prefix(Union(Ctor "sync", Lit VUnit), Unwind("Right", None))))) ] in

    let env = Map.empty in
    let actual = dot max m env "LR" None in

    Assert.True(
        """digraph G {
  "((sync -> Left env={}) ⟦(Set.add sync {})⟧ (sync -> Right env={}) env={})"
  "(Left ⟦(Set.add sync {})⟧ (sync -> Right env={}) env={})"
  "((sync -> Left env={}) ⟦(Set.add sync {})⟧ Right env={})"
  "(Left ⟦(Set.add sync {})⟧ Right env={})"
  "((sync -> Left env={}) ⟦(Set.add sync {})⟧ (sync -> Right env={}) env={})" -> "(Left ⟦(Set.add sync {})⟧ Right env={})" [label="sync"]
  "(Left ⟦(Set.add sync {})⟧ (sync -> Right env={}) env={})" -> "((sync -> Left env={}) ⟦(Set.add sync {})⟧ (sync -> Right env={}) env={})" [label="blue"]
  "((sync -> Left env={}) ⟦(Set.add sync {})⟧ Right env={})" -> "((sync -> Left env={}) ⟦(Set.add sync {})⟧ (sync -> Right env={}) env={})" [label="red"]
  "(Left ⟦(Set.add sync {})⟧ Right env={})" -> "((sync -> Left env={}) ⟦(Set.add sync {})⟧ Right env={})" [label="blue"]
  "(Left ⟦(Set.add sync {})⟧ Right env={})" -> "(Left ⟦(Set.add sync {})⟧ (sync -> Right env={}) env={})" [label="red"]
}""" =
            actual,
        actual
    )

[<Fact>]
let coinToss () =
    let m =
        Map
            [ ("Coin", (None, Prefix(Union(Ctor "toss", Lit VUnit), Unwind("Coin'", None))))
              ("Coin'",
               (None,
                IntCh(
                    Prefix(Union(Ctor "heads", Lit VUnit), Unwind("Coin", None)),
                    Prefix(Union(Ctor "tails", Lit VUnit), Unwind("Coin", None))
                )))
              ("Man", (None, Prefix(Union(Ctor "toss", Lit VUnit), Unwind("Man'", None))))
              ("Man'",
               (None,
                ExtCh(
                    Prefix(Union(Ctor "heads", Lit VUnit), Prefix(Union(Ctor "left", Lit VUnit), Unwind("Man", None))),
                    Prefix(Union(Ctor "tails", Lit VUnit), Prefix(Union(Ctor "right", Lit VUnit), Unwind("Man", None)))
                )))
              ("CoinToss",
               (None,
                InterfaceParallel(
                    Unwind("Coin", None),
                    Lit(
                        VSet(
                            Set
                                [ VUnion(Ctor "toss", VUnit)
                                  VUnion(Ctor "heads", VUnit)
                                  VUnion(Ctor "tails", VUnit) ]
                        )
                    ),
                    Unwind("Man", None)
                ))) ]

    let env = Map.empty in
    let actual = dot max m env "CoinToss" None in

    Assert.True(
        """digraph G {
  "(Coin ⟦{heads, tails, toss}⟧ (left -> Man env={}) env={})"
  "(Coin ⟦{heads, tails, toss}⟧ (right -> Man env={}) env={})"
  "((heads -> Coin env={}) ⟦{heads, tails, toss}⟧ Man' env={})"
  "((tails -> Coin env={}) ⟦{heads, tails, toss}⟧ Man' env={})"
  "(Coin' ⟦{heads, tails, toss}⟧ Man' env={})"
  "(Coin ⟦{heads, tails, toss}⟧ Man env={})"
  "(Coin ⟦{heads, tails, toss}⟧ (left -> Man env={}) env={})" -> "(Coin ⟦{heads, tails, toss}⟧ Man env={})" [label="left"]
  "(Coin ⟦{heads, tails, toss}⟧ (right -> Man env={}) env={})" -> "(Coin ⟦{heads, tails, toss}⟧ Man env={})" [label="right"]
  "((heads -> Coin env={}) ⟦{heads, tails, toss}⟧ Man' env={})" -> "(Coin ⟦{heads, tails, toss}⟧ (left -> Man env={}) env={})" [label="heads"]
  "((tails -> Coin env={}) ⟦{heads, tails, toss}⟧ Man' env={})" -> "(Coin ⟦{heads, tails, toss}⟧ (right -> Man env={}) env={})" [label="tails"]
  "(Coin' ⟦{heads, tails, toss}⟧ Man' env={})" -> "((tails -> Coin env={}) ⟦{heads, tails, toss}⟧ Man' env={})" [label="τ"]
  "(Coin' ⟦{heads, tails, toss}⟧ Man' env={})" -> "((heads -> Coin env={}) ⟦{heads, tails, toss}⟧ Man' env={})" [label="τ"]
  "(Coin ⟦{heads, tails, toss}⟧ Man env={})" -> "(Coin' ⟦{heads, tails, toss}⟧ Man' env={})" [label="toss"]
}""" =
            actual,
        actual
    )

[<Fact>]
let lrh () =
    let m =
        Map
            [ ("LRH",
               (None,
                Hide(
                    InterfaceParallel(
                        Unwind("Left", None),
                        Lit(VSet(Set [ VUnion(Ctor "sync", VUnit) ])),
                        Unwind("Right", None)
                    ),
                    Lit(VSet(Set [ VUnion(Ctor "sync", VUnit) ]))
                )))
              ("Left",
               (None, Prefix(Union(Ctor "blue", Lit VUnit), Prefix(Union(Ctor "sync", Lit VUnit), Unwind("Left", None)))))
              ("Right",
               (None, Prefix(Union(Ctor "red", Lit VUnit), Prefix(Union(Ctor "sync", Lit VUnit), Unwind("Right", None))))) ] in

    let env = Map.empty in
    let actual = dot max m env "LRH" None in

    Assert.True(
        """digraph G {
  "(((sync -> Left env={}) ⟦{sync}⟧ (sync -> Right env={}) env={}) \\ {sync} env={})"
  "((Left ⟦{sync}⟧ (sync -> Right env={}) env={}) \\ {sync} env={})"
  "(((sync -> Left env={}) ⟦{sync}⟧ Right env={}) \\ {sync} env={})"
  "((Left ⟦{sync}⟧ Right env={}) \\ {sync} env={})"
  "(((sync -> Left env={}) ⟦{sync}⟧ (sync -> Right env={}) env={}) \\ {sync} env={})" -> "((Left ⟦{sync}⟧ Right env={}) \\ {sync} env={})" [label="τ (sync)"]
  "((Left ⟦{sync}⟧ (sync -> Right env={}) env={}) \\ {sync} env={})" -> "(((sync -> Left env={}) ⟦{sync}⟧ (sync -> Right env={}) env={}) \\ {sync} env={})" [label="blue"]
  "(((sync -> Left env={}) ⟦{sync}⟧ Right env={}) \\ {sync} env={})" -> "(((sync -> Left env={}) ⟦{sync}⟧ (sync -> Right env={}) env={}) \\ {sync} env={})" [label="red"]
  "((Left ⟦{sync}⟧ Right env={}) \\ {sync} env={})" -> "(((sync -> Left env={}) ⟦{sync}⟧ Right env={}) \\ {sync} env={})" [label="blue"]
  "((Left ⟦{sync}⟧ Right env={}) \\ {sync} env={})" -> "((Left ⟦{sync}⟧ (sync -> Right env={}) env={}) \\ {sync} env={})" [label="red"]
}""" =
            actual,
        actual
    )

[<Fact>]
let hide3 () =
    let m =
        Map
            [ ("P", (None, Hide(Prefix(Union(Ctor "a", Lit VUnit), Skip), Lit(VSet(Set [ VUnion(Ctor "a", VUnit) ]))))) ] in

    let env = Map.empty in
    let actual = dot max m env "P" None in

    Assert.True(
        """digraph G {
  "Ω"
  "(SKIP \\ {a} env={})"
  "((a -> SKIP env={}) \\ {a} env={})"
  "(SKIP \\ {a} env={})" -> "Ω" [label="✓"]
  "((a -> SKIP env={}) \\ {a} env={})" -> "(SKIP \\ {a} env={})" [label="τ (a)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let count () =
    let m: ProcMap<string, string, string> =
        Map
            [ ("COUNT",
               (Some "n",
                ExtCh(
                    Guard(
                        Less((VarRef "n"), Lit(VNat 10u)),
                        Prefix(Union(Ctor "push", Lit VUnit), Unwind("COUNT", Some(Plus((VarRef "n"), Lit(VNat 1u)))))
                    ),
                    Guard(
                        Eq((VarRef "n"), Lit(VNat 10u)),
                        Prefix(Union(Ctor "reset", Lit VUnit), Unwind("COUNT", Some(Lit(VNat 0u))))
                    )
                ))) ] in

    let env = Map.empty in
    let actual = dot max m env "COUNT" (Some(VNat 0u)) in

    Assert.True(
        """digraph G {
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=10} env={n=10}) else STOP) env={n=10}) □ (if (n = 10) then (reset -> COUNT 0 env={n=10} env={n=10}) else STOP) env={n=10}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=9} env={n=9}) else STOP) env={n=9}) □ (if (n = 10) then (reset -> COUNT 0 env={n=9} env={n=9}) else STOP) env={n=9}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=8} env={n=8}) else STOP) env={n=8}) □ (if (n = 10) then (reset -> COUNT 0 env={n=8} env={n=8}) else STOP) env={n=8}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=7} env={n=7}) else STOP) env={n=7}) □ (if (n = 10) then (reset -> COUNT 0 env={n=7} env={n=7}) else STOP) env={n=7}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=6} env={n=6}) else STOP) env={n=6}) □ (if (n = 10) then (reset -> COUNT 0 env={n=6} env={n=6}) else STOP) env={n=6}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=5} env={n=5}) else STOP) env={n=5}) □ (if (n = 10) then (reset -> COUNT 0 env={n=5} env={n=5}) else STOP) env={n=5}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=4} env={n=4}) else STOP) env={n=4}) □ (if (n = 10) then (reset -> COUNT 0 env={n=4} env={n=4}) else STOP) env={n=4}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=3} env={n=3}) else STOP) env={n=3}) □ (if (n = 10) then (reset -> COUNT 0 env={n=3} env={n=3}) else STOP) env={n=3}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=2} env={n=2}) else STOP) env={n=2}) □ (if (n = 10) then (reset -> COUNT 0 env={n=2} env={n=2}) else STOP) env={n=2}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=1} env={n=1}) else STOP) env={n=1}) □ (if (n = 10) then (reset -> COUNT 0 env={n=1} env={n=1}) else STOP) env={n=1}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=0} env={n=0}) else STOP) env={n=0}) □ (if (n = 10) then (reset -> COUNT 0 env={n=0} env={n=0}) else STOP) env={n=0}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=10} env={n=10}) else STOP) env={n=10}) □ (if (n = 10) then (reset -> COUNT 0 env={n=10} env={n=10}) else STOP) env={n=10}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=0} env={n=0}) else STOP) env={n=0}) □ (if (n = 10) then (reset -> COUNT 0 env={n=0} env={n=0}) else STOP) env={n=0}))" [label="reset"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=9} env={n=9}) else STOP) env={n=9}) □ (if (n = 10) then (reset -> COUNT 0 env={n=9} env={n=9}) else STOP) env={n=9}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=10} env={n=10}) else STOP) env={n=10}) □ (if (n = 10) then (reset -> COUNT 0 env={n=10} env={n=10}) else STOP) env={n=10}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=8} env={n=8}) else STOP) env={n=8}) □ (if (n = 10) then (reset -> COUNT 0 env={n=8} env={n=8}) else STOP) env={n=8}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=9} env={n=9}) else STOP) env={n=9}) □ (if (n = 10) then (reset -> COUNT 0 env={n=9} env={n=9}) else STOP) env={n=9}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=7} env={n=7}) else STOP) env={n=7}) □ (if (n = 10) then (reset -> COUNT 0 env={n=7} env={n=7}) else STOP) env={n=7}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=8} env={n=8}) else STOP) env={n=8}) □ (if (n = 10) then (reset -> COUNT 0 env={n=8} env={n=8}) else STOP) env={n=8}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=6} env={n=6}) else STOP) env={n=6}) □ (if (n = 10) then (reset -> COUNT 0 env={n=6} env={n=6}) else STOP) env={n=6}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=7} env={n=7}) else STOP) env={n=7}) □ (if (n = 10) then (reset -> COUNT 0 env={n=7} env={n=7}) else STOP) env={n=7}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=5} env={n=5}) else STOP) env={n=5}) □ (if (n = 10) then (reset -> COUNT 0 env={n=5} env={n=5}) else STOP) env={n=5}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=6} env={n=6}) else STOP) env={n=6}) □ (if (n = 10) then (reset -> COUNT 0 env={n=6} env={n=6}) else STOP) env={n=6}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=4} env={n=4}) else STOP) env={n=4}) □ (if (n = 10) then (reset -> COUNT 0 env={n=4} env={n=4}) else STOP) env={n=4}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=5} env={n=5}) else STOP) env={n=5}) □ (if (n = 10) then (reset -> COUNT 0 env={n=5} env={n=5}) else STOP) env={n=5}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=3} env={n=3}) else STOP) env={n=3}) □ (if (n = 10) then (reset -> COUNT 0 env={n=3} env={n=3}) else STOP) env={n=3}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=4} env={n=4}) else STOP) env={n=4}) □ (if (n = 10) then (reset -> COUNT 0 env={n=4} env={n=4}) else STOP) env={n=4}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=2} env={n=2}) else STOP) env={n=2}) □ (if (n = 10) then (reset -> COUNT 0 env={n=2} env={n=2}) else STOP) env={n=2}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=3} env={n=3}) else STOP) env={n=3}) □ (if (n = 10) then (reset -> COUNT 0 env={n=3} env={n=3}) else STOP) env={n=3}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=1} env={n=1}) else STOP) env={n=1}) □ (if (n = 10) then (reset -> COUNT 0 env={n=1} env={n=1}) else STOP) env={n=1}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=2} env={n=2}) else STOP) env={n=2}) □ (if (n = 10) then (reset -> COUNT 0 env={n=2} env={n=2}) else STOP) env={n=2}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=0} env={n=0}) else STOP) env={n=0}) □ (if (n = 10) then (reset -> COUNT 0 env={n=0} env={n=0}) else STOP) env={n=0}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=1} env={n=1}) else STOP) env={n=1}) □ (if (n = 10) then (reset -> COUNT 0 env={n=1} env={n=1}) else STOP) env={n=1}))" [label="push"]
}""" =
            actual,
        actual
    )

[<Fact>]
let roVarSys1 () =
    let readEvs = Univ(TUnion("read", Map [ (Ctor "Read", TNat) ])) in

    let m: ProcMap<string, string, string> =
        Map
            [ ("ROVarSys1",
               (None,
                InterfaceParallel(
                    Unwind("ROVar", Some(Lit(VNat 0u))),
                    readEvs,
                    InterfaceParallel(
                        Unwind("Reader1", None),
                        readEvs,
                        InterfaceParallel(Unwind("Reader2", None), readEvs, Unwind("Reader3", None))
                    )
                )))
              ("ROVar",
               (Some "x",
                Prefix(
                    Union(Ctor "Read", VarRef "x"),
                    Unwind(
                        "ROVar",
                        Some(Expr.If(Less(VarRef "x", Lit(VNat 4u)), Plus(VarRef "x", Lit(VNat 1u)), Lit(VNat 0u)))
                    )
                )))
              ("Reader1", (None, PrefixRecv(readEvs, "x", Stop)))
              ("Reader2", (None, PrefixRecv(readEvs, "x", Stop)))
              ("Reader3", (None, PrefixRecv(readEvs, "x", Stop))) ] in

    let env = Map.empty in
    let actual = dot max m env "ROVarSys1" None in

    Assert.True(
        """digraph G {
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (STOP ⟦(univ::read)⟧ (STOP ⟦(univ::read)⟧ STOP env={}) env={}) env={})"  [fillcolor=red, style=filled, fontcolor=white]
  "(ROVar 0 env={} ⟦(univ::read)⟧ (Reader1 ⟦(univ::read)⟧ (Reader2 ⟦(univ::read)⟧ Reader3 env={}) env={}) env={})"
  "(ROVar 0 env={} ⟦(univ::read)⟧ (Reader1 ⟦(univ::read)⟧ (Reader2 ⟦(univ::read)⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (STOP ⟦(univ::read)⟧ (STOP ⟦(univ::read)⟧ STOP env={}) env={}) env={})" [label="(Read 0)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let roVarSys2 () =
    let readEvs = Univ(TUnion("read", Map [ (Ctor "Read", TNat) ])) in

    let m: ProcMap<string, string, string> =
        Map
            [ ("ROVarSys2",
               (None,
                InterfaceParallel(
                    Unwind("ROVar", Some(Lit(VNat 0u))),
                    readEvs,
                    Interleave(Unwind("Reader1", None), Interleave(Unwind("Reader2", None), Unwind("Reader3", None)))
                )))
              ("ROVar",
               (Some "x",
                Prefix(
                    Union(Ctor "Read", VarRef "x"),
                    Unwind(
                        "ROVar",
                        Some(Expr.If(Less(VarRef "x", Lit(VNat 4u)), Plus(VarRef "x", Lit(VNat 1u)), Lit(VNat 0u)))
                    )
                )))
              ("Reader1", (None, PrefixRecv(readEvs, "x", Stop)))
              ("Reader2", (None, PrefixRecv(readEvs, "x", Stop)))
              ("Reader3", (None, PrefixRecv(readEvs, "x", Stop))) ] in

    let env = Map.empty in
    let actual = dot max m env "ROVarSys2" None in

    Assert.True(
        """digraph G {
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=2} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP env={}) env={}) env={})"  [fillcolor=red, style=filled, fontcolor=white]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP env={}) env={}) env={})"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3 env={}) env={}) env={})"
  "(ROVar 0 env={} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=2} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP env={}) env={}) env={})" [label="(Read 2)"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=2} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP env={}) env={}) env={})" [label="(Read 2)"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=2} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP env={}) env={}) env={})" [label="(Read 2)"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3 env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3 env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar 0 env={} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3 env={}) env={}) env={})" [label="(Read 0)"]
  "(ROVar 0 env={} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP env={}) env={}) env={})" [label="(Read 0)"]
  "(ROVar 0 env={} ⟦(univ::read)⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦(univ::read)⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3 env={}) env={}) env={})" [label="(Read 0)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let testMax () =
    let m =
        Map
            [ ("P",
               (Some "n",
                ExtCh(
                    Prefix(Union(Ctor "ch", Lit VUnit), Unwind("P", Some(Plus(VarRef "n", Lit(VNat 1u))))),
                    Prefix(Union(Ctor "ch", Lit VUnit), Unwind("P", Some(Minus(VarRef "n", Lit(VNat 1u)))))
                ))) ] in

    let env = Map.empty in
    let actual = dot 10 m env "P" (Some(VNat 0u)) in

    Assert.True(
        """digraph G {
  "((ch -> P (n + 1) env={n=9} env={n=9}) □ (ch -> P (n - 1) env={n=9} env={n=9}))"
  "((ch -> P (n + 1) env={n=8} env={n=8}) □ (ch -> P (n - 1) env={n=8} env={n=8}))"
  "((ch -> P (n + 1) env={n=7} env={n=7}) □ (ch -> P (n - 1) env={n=7} env={n=7}))"
  "((ch -> P (n + 1) env={n=6} env={n=6}) □ (ch -> P (n - 1) env={n=6} env={n=6}))"
  "((ch -> P (n + 1) env={n=5} env={n=5}) □ (ch -> P (n - 1) env={n=5} env={n=5}))"
  "((ch -> P (n + 1) env={n=4} env={n=4}) □ (ch -> P (n - 1) env={n=4} env={n=4}))"
  "((ch -> P (n + 1) env={n=3} env={n=3}) □ (ch -> P (n - 1) env={n=3} env={n=3}))"
  "((ch -> P (n + 1) env={n=2} env={n=2}) □ (ch -> P (n - 1) env={n=2} env={n=2}))"
  "((ch -> P (n + 1) env={n=1} env={n=1}) □ (ch -> P (n - 1) env={n=1} env={n=1}))"
  "((ch -> P (n + 1) env={n=0} env={n=0}) □ (ch -> P (n - 1) env={n=0} env={n=0}))"
  "((ch -> P (n + 1) env={n=9} env={n=9}) □ (ch -> P (n - 1) env={n=9} env={n=9}))" -> "((ch -> P (n + 1) env={n=10} env={n=10}) □ (ch -> P (n - 1) env={n=10} env={n=10}))" [label="ch"]
  "((ch -> P (n + 1) env={n=9} env={n=9}) □ (ch -> P (n - 1) env={n=9} env={n=9}))" -> "((ch -> P (n + 1) env={n=8} env={n=8}) □ (ch -> P (n - 1) env={n=8} env={n=8}))" [label="ch"]
  "((ch -> P (n + 1) env={n=8} env={n=8}) □ (ch -> P (n - 1) env={n=8} env={n=8}))" -> "((ch -> P (n + 1) env={n=9} env={n=9}) □ (ch -> P (n - 1) env={n=9} env={n=9}))" [label="ch"]
  "((ch -> P (n + 1) env={n=8} env={n=8}) □ (ch -> P (n - 1) env={n=8} env={n=8}))" -> "((ch -> P (n + 1) env={n=7} env={n=7}) □ (ch -> P (n - 1) env={n=7} env={n=7}))" [label="ch"]
  "((ch -> P (n + 1) env={n=7} env={n=7}) □ (ch -> P (n - 1) env={n=7} env={n=7}))" -> "((ch -> P (n + 1) env={n=8} env={n=8}) □ (ch -> P (n - 1) env={n=8} env={n=8}))" [label="ch"]
  "((ch -> P (n + 1) env={n=7} env={n=7}) □ (ch -> P (n - 1) env={n=7} env={n=7}))" -> "((ch -> P (n + 1) env={n=6} env={n=6}) □ (ch -> P (n - 1) env={n=6} env={n=6}))" [label="ch"]
  "((ch -> P (n + 1) env={n=6} env={n=6}) □ (ch -> P (n - 1) env={n=6} env={n=6}))" -> "((ch -> P (n + 1) env={n=7} env={n=7}) □ (ch -> P (n - 1) env={n=7} env={n=7}))" [label="ch"]
  "((ch -> P (n + 1) env={n=6} env={n=6}) □ (ch -> P (n - 1) env={n=6} env={n=6}))" -> "((ch -> P (n + 1) env={n=5} env={n=5}) □ (ch -> P (n - 1) env={n=5} env={n=5}))" [label="ch"]
  "((ch -> P (n + 1) env={n=5} env={n=5}) □ (ch -> P (n - 1) env={n=5} env={n=5}))" -> "((ch -> P (n + 1) env={n=6} env={n=6}) □ (ch -> P (n - 1) env={n=6} env={n=6}))" [label="ch"]
  "((ch -> P (n + 1) env={n=5} env={n=5}) □ (ch -> P (n - 1) env={n=5} env={n=5}))" -> "((ch -> P (n + 1) env={n=4} env={n=4}) □ (ch -> P (n - 1) env={n=4} env={n=4}))" [label="ch"]
  "((ch -> P (n + 1) env={n=4} env={n=4}) □ (ch -> P (n - 1) env={n=4} env={n=4}))" -> "((ch -> P (n + 1) env={n=5} env={n=5}) □ (ch -> P (n - 1) env={n=5} env={n=5}))" [label="ch"]
  "((ch -> P (n + 1) env={n=4} env={n=4}) □ (ch -> P (n - 1) env={n=4} env={n=4}))" -> "((ch -> P (n + 1) env={n=3} env={n=3}) □ (ch -> P (n - 1) env={n=3} env={n=3}))" [label="ch"]
  "((ch -> P (n + 1) env={n=3} env={n=3}) □ (ch -> P (n - 1) env={n=3} env={n=3}))" -> "((ch -> P (n + 1) env={n=4} env={n=4}) □ (ch -> P (n - 1) env={n=4} env={n=4}))" [label="ch"]
  "((ch -> P (n + 1) env={n=3} env={n=3}) □ (ch -> P (n - 1) env={n=3} env={n=3}))" -> "((ch -> P (n + 1) env={n=2} env={n=2}) □ (ch -> P (n - 1) env={n=2} env={n=2}))" [label="ch"]
  "((ch -> P (n + 1) env={n=2} env={n=2}) □ (ch -> P (n - 1) env={n=2} env={n=2}))" -> "((ch -> P (n + 1) env={n=3} env={n=3}) □ (ch -> P (n - 1) env={n=3} env={n=3}))" [label="ch"]
  "((ch -> P (n + 1) env={n=2} env={n=2}) □ (ch -> P (n - 1) env={n=2} env={n=2}))" -> "((ch -> P (n + 1) env={n=1} env={n=1}) □ (ch -> P (n - 1) env={n=1} env={n=1}))" [label="ch"]
  "((ch -> P (n + 1) env={n=1} env={n=1}) □ (ch -> P (n - 1) env={n=1} env={n=1}))" -> "((ch -> P (n + 1) env={n=2} env={n=2}) □ (ch -> P (n - 1) env={n=2} env={n=2}))" [label="ch"]
  "((ch -> P (n + 1) env={n=1} env={n=1}) □ (ch -> P (n - 1) env={n=1} env={n=1}))" -> "((ch -> P (n + 1) env={n=0} env={n=0}) □ (ch -> P (n - 1) env={n=0} env={n=0}))" [label="ch"]
  "((ch -> P (n + 1) env={n=0} env={n=0}) □ (ch -> P (n - 1) env={n=0} env={n=0}))" -> "((ch -> P (n + 1) env={n=1} env={n=1}) □ (ch -> P (n - 1) env={n=1} env={n=1}))" [label="ch"]
  "((ch -> P (n + 1) env={n=0} env={n=0}) □ (ch -> P (n - 1) env={n=0} env={n=0}))" -> "((ch -> P (n + 1) env={n=0} env={n=0}) □ (ch -> P (n - 1) env={n=0} env={n=0}))" [label="ch"]
}""" =
            actual,
        actual
    )
