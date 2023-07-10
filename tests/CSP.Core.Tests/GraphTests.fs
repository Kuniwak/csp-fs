module CSP.Core.Tests.GraphTests

open Xunit
open CSP.Core.EventSpec
open CSP.Core.Val
open CSP.Core.Type
open CSP.Core.Expr
open CSP.Core.ProcMap
open CSP.Core.Proc
open CSP.Core.Graph

let max = 100

[<Fact>]
let abSkip () =
    let m: Map<string, Unit option * Proc<string, string, Unit, Unit, Unit>> =
        Map
            [ ("ABSkip", (None, Seq(Unwind("ASkip", None), Unwind("BSkip", None))))
              ("ASkip", (None, Prefix("a", Skip)))
              ("BSkip", (None, Prefix("b", Skip))) ]

    let env = Map.empty in
    let actual = dot max m env "ABSkip" None in

    Assert.True(
        """digraph G {
  "Ω"
  "SKIP"
  "(b -> SKIP)"
  "(SKIP ; BSkip)"
  "(ASkip ; BSkip)"
  "SKIP" -> "Ω" [label="✓"]
  "(b -> SKIP)" -> "SKIP" [label="b"]
  "(SKIP ; BSkip)" -> "(b -> SKIP)" [label="τ"]
  "(ASkip ; BSkip)" -> "(SKIP ; BSkip)" [label="a"]
}""" =
            actual,
        actual
    )

[<Fact>]
let parABC () =
    let m: ProcMap<string, string, Unit, Unit, Unit> =
        Map
            [ ("ParABC",
               (None,
                InterfaceParallel(
                    Prefix("a", Skip),
                    Set.empty,
                    InterfaceParallel(Prefix("b", Skip), Set.empty, Prefix("c", Skip))
                )))
              ("P", (None, Seq(Unwind("ParABC", None), Prefix("d", Skip)))) ] in

    let env = Map.empty in
    let actual = dot max m env "P" None in

    Assert.True(
        """digraph G {
  "Ω"
  "SKIP"
  "(d -> SKIP)"
  "((Ω ⟦{}⟧ Ω) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ Ω) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ Ω) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))"
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))"
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))"
  "(ParABC ; (d -> SKIP))"
  "SKIP" -> "Ω" [label="✓"]
  "(d -> SKIP)" -> "SKIP" [label="d"]
  "((Ω ⟦{}⟧ Ω) ; (d -> SKIP))" -> "(d -> SKIP)" [label="τ"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ Ω) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ Ω) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ Ω) ; (d -> SKIP))" [label="τ"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ Ω) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ Ω) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ Ω) ; (d -> SKIP))" [label="a"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="b"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="τ"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ Ω) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="a"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="b"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="b"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="τ"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="a"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="a"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="b"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="b"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="b"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="a"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="τ"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="a"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="a"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="b"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="b"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="a"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="τ"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="a"]
  "(ParABC ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="b"]
  "(ParABC ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="c"]
  "(ParABC ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="a"]
}""" =
            actual,
        actual
    )

[<Fact>]
let rand2 () =
    let m: ProcMap<string, int, Unit, Unit, Unit> =
        Map [ ("P", (None, IntCh(Prefix(1, Unwind("P", None)), Prefix(2, Unwind("P", None))))) ] in

    let env = Map.empty in
    let actual = dot max m env "P" None in

    Assert.True(
        """digraph G {
  "(2 -> P)"
  "(1 -> P)"
  "((1 -> P) ⨅ (2 -> P))"
  "(2 -> P)" -> "((1 -> P) ⨅ (2 -> P))" [label="2"]
  "(1 -> P)" -> "((1 -> P) ⨅ (2 -> P))" [label="1"]
  "((1 -> P) ⨅ (2 -> P))" -> "(1 -> P)" [label="τ"]
  "((1 -> P) ⨅ (2 -> P))" -> "(2 -> P)" [label="τ"]
}""" =
            actual,
        actual
    )

[<Fact>]
let abs () =
    let m: ProcMap<string, string, Unit, Unit, Unit> =
        Map
            [ ("ABS",
               (None,
                ExtCh(IntCh(Prefix("a", Unwind("ABS", None)), Prefix("b", Unwind("ABS", None))), Prefix("s", Stop)))) ] in

    let env = Map.empty in
    let actual = dot max m env "ABS" None in

    Assert.True(
        """digraph G {
  "STOP"  [fillcolor=red, style=filled, fontcolor=white]
  "((a -> ABS) □ (s -> STOP))"
  "((b -> ABS) □ (s -> STOP))"
  "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))"
  "((a -> ABS) □ (s -> STOP))" -> "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" [label="a"]
  "((a -> ABS) □ (s -> STOP))" -> "STOP" [label="s"]
  "((b -> ABS) □ (s -> STOP))" -> "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" [label="b"]
  "((b -> ABS) □ (s -> STOP))" -> "STOP" [label="s"]
  "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" -> "((b -> ABS) □ (s -> STOP))" [label="τ"]
  "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" -> "((a -> ABS) □ (s -> STOP))" [label="τ"]
  "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" -> "STOP" [label="s"]
}""" =
            actual,
        actual
    )

[<Fact>]
let lr () =
    let m: ProcMap<string, string, Unit, Unit, Unit> =
        Map
            [ ("LR", (None, InterfaceParallel(Unwind("Left", None), Set [ Event "sync" ], Unwind("Right", None))))
              ("Left", (None, Prefix("blue", Prefix("sync", Unwind("Left", None)))))
              ("Right", (None, Prefix("red", Prefix("sync", Unwind("Right", None))))) ] in

    let env = Map.empty in
    let actual = dot max m env "LR" None in

    Assert.True(
        """digraph G {
  "((sync -> Left) ⟦{sync}⟧ (sync -> Right))"
  "(Left ⟦{sync}⟧ (sync -> Right))"
  "((sync -> Left) ⟦{sync}⟧ Right)"
  "(Left ⟦{sync}⟧ Right)"
  "((sync -> Left) ⟦{sync}⟧ (sync -> Right))" -> "(Left ⟦{sync}⟧ Right)" [label="sync"]
  "(Left ⟦{sync}⟧ (sync -> Right))" -> "((sync -> Left) ⟦{sync}⟧ (sync -> Right))" [label="blue"]
  "((sync -> Left) ⟦{sync}⟧ Right)" -> "((sync -> Left) ⟦{sync}⟧ (sync -> Right))" [label="red"]
  "(Left ⟦{sync}⟧ Right)" -> "((sync -> Left) ⟦{sync}⟧ Right)" [label="blue"]
  "(Left ⟦{sync}⟧ Right)" -> "(Left ⟦{sync}⟧ (sync -> Right))" [label="red"]
}""" =
            actual,
        actual
    )

[<Fact>]
let coinToss () =
    let m =
        Map
            [ ("Coin", (None, Prefix("toss", Unwind("Coin'", None))))
              ("Coin'", (None, IntCh(Prefix("heads", Unwind("Coin", None)), Prefix("tails", Unwind("Coin", None)))))
              ("Man", (None, Prefix("toss", Unwind("Man'", None))))
              ("Man'",
               (None,
                ExtCh(
                    Prefix("heads", Prefix("left", Unwind("Man", None))),
                    Prefix("tails", Prefix("right", Unwind("Man", None)))
                )))
              ("CoinToss",
               (None,
                InterfaceParallel(
                    Unwind("Coin", None),
                    Set [ Event "toss"; Event "heads"; Event "tails" ],
                    Unwind("Man", None)
                ))) ]

    let env = Map.empty in
    let actual = dot max m env "CoinToss" None in

    Assert.True(
        """digraph G {
  "(Coin ⟦{heads, tails, toss}⟧ (left -> Man))"
  "(Coin ⟦{heads, tails, toss}⟧ (right -> Man))"
  "((heads -> Coin) ⟦{heads, tails, toss}⟧ Man')"
  "((tails -> Coin) ⟦{heads, tails, toss}⟧ Man')"
  "(Coin' ⟦{heads, tails, toss}⟧ Man')"
  "(Coin ⟦{heads, tails, toss}⟧ Man)"
  "(Coin ⟦{heads, tails, toss}⟧ (left -> Man))" -> "(Coin ⟦{heads, tails, toss}⟧ Man)" [label="left"]
  "(Coin ⟦{heads, tails, toss}⟧ (right -> Man))" -> "(Coin ⟦{heads, tails, toss}⟧ Man)" [label="right"]
  "((heads -> Coin) ⟦{heads, tails, toss}⟧ Man')" -> "(Coin ⟦{heads, tails, toss}⟧ (left -> Man))" [label="heads"]
  "((tails -> Coin) ⟦{heads, tails, toss}⟧ Man')" -> "(Coin ⟦{heads, tails, toss}⟧ (right -> Man))" [label="tails"]
  "(Coin' ⟦{heads, tails, toss}⟧ Man')" -> "((tails -> Coin) ⟦{heads, tails, toss}⟧ Man')" [label="τ"]
  "(Coin' ⟦{heads, tails, toss}⟧ Man')" -> "((heads -> Coin) ⟦{heads, tails, toss}⟧ Man')" [label="τ"]
  "(Coin ⟦{heads, tails, toss}⟧ Man)" -> "(Coin' ⟦{heads, tails, toss}⟧ Man')" [label="toss"]
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
                    InterfaceParallel(Unwind("Left", None), Set [ Event "sync" ], Unwind("Right", None)),
                    Set [ Event "sync" ]
                )))
              ("Left", (None, Prefix("blue", Prefix("sync", Unwind("Left", None)))))
              ("Right", (None, Prefix("red", Prefix("sync", Unwind("Right", None))))) ] in

    let env = Map.empty in
    let actual = dot max m env "LRH" None in

    Assert.True(
        """digraph G {
  "(((sync -> Left) ⟦{sync}⟧ (sync -> Right)) \\ {sync})"
  "((Left ⟦{sync}⟧ (sync -> Right)) \\ {sync})"
  "(((sync -> Left) ⟦{sync}⟧ Right) \\ {sync})"
  "((Left ⟦{sync}⟧ Right) \\ {sync})"
  "(((sync -> Left) ⟦{sync}⟧ (sync -> Right)) \\ {sync})" -> "((Left ⟦{sync}⟧ Right) \\ {sync})" [label="τ (sync)"]
  "((Left ⟦{sync}⟧ (sync -> Right)) \\ {sync})" -> "(((sync -> Left) ⟦{sync}⟧ (sync -> Right)) \\ {sync})" [label="blue"]
  "(((sync -> Left) ⟦{sync}⟧ Right) \\ {sync})" -> "(((sync -> Left) ⟦{sync}⟧ (sync -> Right)) \\ {sync})" [label="red"]
  "((Left ⟦{sync}⟧ Right) \\ {sync})" -> "(((sync -> Left) ⟦{sync}⟧ Right) \\ {sync})" [label="blue"]
  "((Left ⟦{sync}⟧ Right) \\ {sync})" -> "((Left ⟦{sync}⟧ (sync -> Right)) \\ {sync})" [label="red"]
}""" =
            actual,
        actual
    )

[<Fact>]
let hide3 () =
    let m = Map [ ("P", (None, Hide(Prefix("a", Skip), Set [ Event "a" ]))) ] in
    let env = Map.empty in
    let actual = dot max m env "P" None in

    Assert.True(
        """digraph G {
  "Ω"
  "(SKIP \\ {a})"
  "((a -> SKIP) \\ {a})"
  "(SKIP \\ {a})" -> "Ω" [label="✓"]
  "((a -> SKIP) \\ {a})" -> "(SKIP \\ {a})" [label="τ (a)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let count () =
    let m: ProcMap<string, string, Unit, string, Unit> =
        Map
            [ ("COUNT",
               (Some "n",
                ExtCh(
                    Guard(
                        Less((VarRef "n"), (LitNat 10u)),
                        Prefix("push", Unwind("COUNT", Some(Plus((VarRef "n"), (LitNat 1u)))))
                    ),
                    Guard(Eq((VarRef "n"), (LitNat 10u)), Prefix("reset", Unwind("COUNT", Some(LitNat 0u))))
                ))) ] in

    let env = Map.empty in
    let actual = dot max m env "COUNT" (Some(VNat 0u)) in

    Assert.True(
        """digraph G {
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=10}) else STOP) env={n=10}) □ (if (n = 10) then (reset -> COUNT 0 env={n=10}) else STOP) env={n=10}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=9}) else STOP) env={n=9}) □ (if (n = 10) then (reset -> COUNT 0 env={n=9}) else STOP) env={n=9}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=8}) else STOP) env={n=8}) □ (if (n = 10) then (reset -> COUNT 0 env={n=8}) else STOP) env={n=8}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=7}) else STOP) env={n=7}) □ (if (n = 10) then (reset -> COUNT 0 env={n=7}) else STOP) env={n=7}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=6}) else STOP) env={n=6}) □ (if (n = 10) then (reset -> COUNT 0 env={n=6}) else STOP) env={n=6}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=5}) else STOP) env={n=5}) □ (if (n = 10) then (reset -> COUNT 0 env={n=5}) else STOP) env={n=5}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=4}) else STOP) env={n=4}) □ (if (n = 10) then (reset -> COUNT 0 env={n=4}) else STOP) env={n=4}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=3}) else STOP) env={n=3}) □ (if (n = 10) then (reset -> COUNT 0 env={n=3}) else STOP) env={n=3}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=2}) else STOP) env={n=2}) □ (if (n = 10) then (reset -> COUNT 0 env={n=2}) else STOP) env={n=2}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=1}) else STOP) env={n=1}) □ (if (n = 10) then (reset -> COUNT 0 env={n=1}) else STOP) env={n=1}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=0}) else STOP) env={n=0}) □ (if (n = 10) then (reset -> COUNT 0 env={n=0}) else STOP) env={n=0}))"
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=10}) else STOP) env={n=10}) □ (if (n = 10) then (reset -> COUNT 0 env={n=10}) else STOP) env={n=10}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=0}) else STOP) env={n=0}) □ (if (n = 10) then (reset -> COUNT 0 env={n=0}) else STOP) env={n=0}))" [label="reset"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=9}) else STOP) env={n=9}) □ (if (n = 10) then (reset -> COUNT 0 env={n=9}) else STOP) env={n=9}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=10}) else STOP) env={n=10}) □ (if (n = 10) then (reset -> COUNT 0 env={n=10}) else STOP) env={n=10}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=8}) else STOP) env={n=8}) □ (if (n = 10) then (reset -> COUNT 0 env={n=8}) else STOP) env={n=8}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=9}) else STOP) env={n=9}) □ (if (n = 10) then (reset -> COUNT 0 env={n=9}) else STOP) env={n=9}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=7}) else STOP) env={n=7}) □ (if (n = 10) then (reset -> COUNT 0 env={n=7}) else STOP) env={n=7}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=8}) else STOP) env={n=8}) □ (if (n = 10) then (reset -> COUNT 0 env={n=8}) else STOP) env={n=8}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=6}) else STOP) env={n=6}) □ (if (n = 10) then (reset -> COUNT 0 env={n=6}) else STOP) env={n=6}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=7}) else STOP) env={n=7}) □ (if (n = 10) then (reset -> COUNT 0 env={n=7}) else STOP) env={n=7}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=5}) else STOP) env={n=5}) □ (if (n = 10) then (reset -> COUNT 0 env={n=5}) else STOP) env={n=5}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=6}) else STOP) env={n=6}) □ (if (n = 10) then (reset -> COUNT 0 env={n=6}) else STOP) env={n=6}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=4}) else STOP) env={n=4}) □ (if (n = 10) then (reset -> COUNT 0 env={n=4}) else STOP) env={n=4}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=5}) else STOP) env={n=5}) □ (if (n = 10) then (reset -> COUNT 0 env={n=5}) else STOP) env={n=5}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=3}) else STOP) env={n=3}) □ (if (n = 10) then (reset -> COUNT 0 env={n=3}) else STOP) env={n=3}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=4}) else STOP) env={n=4}) □ (if (n = 10) then (reset -> COUNT 0 env={n=4}) else STOP) env={n=4}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=2}) else STOP) env={n=2}) □ (if (n = 10) then (reset -> COUNT 0 env={n=2}) else STOP) env={n=2}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=3}) else STOP) env={n=3}) □ (if (n = 10) then (reset -> COUNT 0 env={n=3}) else STOP) env={n=3}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=1}) else STOP) env={n=1}) □ (if (n = 10) then (reset -> COUNT 0 env={n=1}) else STOP) env={n=1}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=2}) else STOP) env={n=2}) □ (if (n = 10) then (reset -> COUNT 0 env={n=2}) else STOP) env={n=2}))" [label="push"]
  "((if (n < 10) then (push -> COUNT (n + 1) env={n=0}) else STOP) env={n=0}) □ (if (n = 10) then (reset -> COUNT 0 env={n=0}) else STOP) env={n=0}))" -> "((if (n < 10) then (push -> COUNT (n + 1) env={n=1}) else STOP) env={n=1}) □ (if (n = 10) then (reset -> COUNT 0 env={n=1}) else STOP) env={n=1}))" [label="push"]
}""" =
            actual,
        actual
    )

[<Fact>]
let roVarSys1 () =
    let syncR = Set [ Chan "read" ]

    let m: ProcMap<string, Unit, string, string, Unit> =
        Map
            [ ("ROVarSys1",
               (None,
                InterfaceParallel(
                    Unwind("ROVar", Some(LitNat 0u)),
                    syncR,
                    InterfaceParallel(
                        Unwind("Reader1", None),
                        syncR,
                        InterfaceParallel(Unwind("Reader2", None), syncR, Unwind("Reader3", None))
                    )
                )))
              ("ROVar",
               (Some "x",
                PrefixSend(
                    "read",
                    VarRef "x",
                    Unwind("ROVar", Some(Expr.If(Less(VarRef "x", LitNat 4u), Plus(VarRef "x", LitNat 1u), LitNat 0u)))
                )))
              ("Reader1", (None, PrefixRecv("read", "x", TNat, Stop)))
              ("Reader2", (None, PrefixRecv("read", "x", TNat, Stop)))
              ("Reader3", (None, PrefixRecv("read", "x", TNat, Stop))) ] in

    let env = Map.empty in
    let actual = dot max m env "ROVarSys1" None in

    Assert.True(
        """digraph G {
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (STOP ⟦⦃read⦄⟧ (STOP ⟦⦃read⦄⟧ STOP)))"  [fillcolor=red, style=filled, fontcolor=white]
  "(ROVar 0 env={} ⟦⦃read⦄⟧ (Reader1 ⟦⦃read⦄⟧ (Reader2 ⟦⦃read⦄⟧ Reader3)))"
  "(ROVar 0 env={} ⟦⦃read⦄⟧ (Reader1 ⟦⦃read⦄⟧ (Reader2 ⟦⦃read⦄⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (STOP ⟦⦃read⦄⟧ (STOP ⟦⦃read⦄⟧ STOP)))" [label="read.0"]
}""" =
            actual,
        actual
    )

[<Fact>]
let roVarSys2 () =
    let syncR = Set [ Chan "read" ]

    let m: ProcMap<string, Unit, string, string, Unit> =
        Map
            [ ("ROVarSys2",
               (None,
                InterfaceParallel(
                    Unwind("ROVar", Some(LitNat 0u)),
                    syncR,
                    Interleave(Unwind("Reader1", None), Interleave(Unwind("Reader2", None), Unwind("Reader3", None)))
                )))
              ("ROVar",
               (Some "x",
                PrefixSend(
                    "read",
                    VarRef "x",
                    Unwind("ROVar", Some(Expr.If(Less(VarRef "x", LitNat 4u), Plus(VarRef "x", LitNat 1u), LitNat 0u)))
                )))
              ("Reader1", (None, PrefixRecv("read", "x", TNat, Stop)))
              ("Reader2", (None, PrefixRecv("read", "x", TNat, Stop)))
              ("Reader3", (None, PrefixRecv("read", "x", TNat, Stop))) ] in

    let env = Map.empty in
    let actual = dot max m env "ROVarSys2" None in

    Assert.True(
        """digraph G {
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=2} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP)))"  [fillcolor=red, style=filled, fontcolor=white]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP)))"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))"
  "(ROVar 0 env={} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))"
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=2} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="read.2"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=2} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="read.2"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=2} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="read.2"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" [label="read.1"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" [label="read.1"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="read.1"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" [label="read.1"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="read.1"]
  "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=1} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" [label="read.1"]
  "(ROVar 0 env={} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" [label="read.0"]
  "(ROVar 0 env={} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" [label="read.0"]
  "(ROVar 0 env={} ⟦⦃read⦄⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" -> "(ROVar (if (x < 4) then (x + 1) else 0) env={x=0} ⟦⦃read⦄⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" [label="read.0"]
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
                    Prefix("ch", Unwind("P", Some(Plus(VarRef "n", LitNat 1u)))),
                    Prefix("ch", Unwind("P", Some(Plus(VarRef "n", LitNat 1u))))
                ))) ] in

    let env = Map.empty in
    let actual = dot 10 m env "P" (Some(VNat 0u)) in
    Assert.True("""digraph G {
  "((ch -> P (n + 1) env={n=9}) □ (ch -> P (n + 1) env={n=9}))"
  "((ch -> P (n + 1) env={n=8}) □ (ch -> P (n + 1) env={n=8}))"
  "((ch -> P (n + 1) env={n=7}) □ (ch -> P (n + 1) env={n=7}))"
  "((ch -> P (n + 1) env={n=6}) □ (ch -> P (n + 1) env={n=6}))"
  "((ch -> P (n + 1) env={n=5}) □ (ch -> P (n + 1) env={n=5}))"
  "((ch -> P (n + 1) env={n=4}) □ (ch -> P (n + 1) env={n=4}))"
  "((ch -> P (n + 1) env={n=3}) □ (ch -> P (n + 1) env={n=3}))"
  "((ch -> P (n + 1) env={n=2}) □ (ch -> P (n + 1) env={n=2}))"
  "((ch -> P (n + 1) env={n=1}) □ (ch -> P (n + 1) env={n=1}))"
  "((ch -> P (n + 1) env={n=0}) □ (ch -> P (n + 1) env={n=0}))"
  "((ch -> P (n + 1) env={n=9}) □ (ch -> P (n + 1) env={n=9}))" -> "((ch -> P (n + 1) env={n=10}) □ (ch -> P (n + 1) env={n=10}))" [label="ch"]
  "((ch -> P (n + 1) env={n=9}) □ (ch -> P (n + 1) env={n=9}))" -> "((ch -> P (n + 1) env={n=10}) □ (ch -> P (n + 1) env={n=10}))" [label="ch"]
  "((ch -> P (n + 1) env={n=8}) □ (ch -> P (n + 1) env={n=8}))" -> "((ch -> P (n + 1) env={n=9}) □ (ch -> P (n + 1) env={n=9}))" [label="ch"]
  "((ch -> P (n + 1) env={n=8}) □ (ch -> P (n + 1) env={n=8}))" -> "((ch -> P (n + 1) env={n=9}) □ (ch -> P (n + 1) env={n=9}))" [label="ch"]
  "((ch -> P (n + 1) env={n=7}) □ (ch -> P (n + 1) env={n=7}))" -> "((ch -> P (n + 1) env={n=8}) □ (ch -> P (n + 1) env={n=8}))" [label="ch"]
  "((ch -> P (n + 1) env={n=7}) □ (ch -> P (n + 1) env={n=7}))" -> "((ch -> P (n + 1) env={n=8}) □ (ch -> P (n + 1) env={n=8}))" [label="ch"]
  "((ch -> P (n + 1) env={n=6}) □ (ch -> P (n + 1) env={n=6}))" -> "((ch -> P (n + 1) env={n=7}) □ (ch -> P (n + 1) env={n=7}))" [label="ch"]
  "((ch -> P (n + 1) env={n=6}) □ (ch -> P (n + 1) env={n=6}))" -> "((ch -> P (n + 1) env={n=7}) □ (ch -> P (n + 1) env={n=7}))" [label="ch"]
  "((ch -> P (n + 1) env={n=5}) □ (ch -> P (n + 1) env={n=5}))" -> "((ch -> P (n + 1) env={n=6}) □ (ch -> P (n + 1) env={n=6}))" [label="ch"]
  "((ch -> P (n + 1) env={n=5}) □ (ch -> P (n + 1) env={n=5}))" -> "((ch -> P (n + 1) env={n=6}) □ (ch -> P (n + 1) env={n=6}))" [label="ch"]
  "((ch -> P (n + 1) env={n=4}) □ (ch -> P (n + 1) env={n=4}))" -> "((ch -> P (n + 1) env={n=5}) □ (ch -> P (n + 1) env={n=5}))" [label="ch"]
  "((ch -> P (n + 1) env={n=4}) □ (ch -> P (n + 1) env={n=4}))" -> "((ch -> P (n + 1) env={n=5}) □ (ch -> P (n + 1) env={n=5}))" [label="ch"]
  "((ch -> P (n + 1) env={n=3}) □ (ch -> P (n + 1) env={n=3}))" -> "((ch -> P (n + 1) env={n=4}) □ (ch -> P (n + 1) env={n=4}))" [label="ch"]
  "((ch -> P (n + 1) env={n=3}) □ (ch -> P (n + 1) env={n=3}))" -> "((ch -> P (n + 1) env={n=4}) □ (ch -> P (n + 1) env={n=4}))" [label="ch"]
  "((ch -> P (n + 1) env={n=2}) □ (ch -> P (n + 1) env={n=2}))" -> "((ch -> P (n + 1) env={n=3}) □ (ch -> P (n + 1) env={n=3}))" [label="ch"]
  "((ch -> P (n + 1) env={n=2}) □ (ch -> P (n + 1) env={n=2}))" -> "((ch -> P (n + 1) env={n=3}) □ (ch -> P (n + 1) env={n=3}))" [label="ch"]
  "((ch -> P (n + 1) env={n=1}) □ (ch -> P (n + 1) env={n=1}))" -> "((ch -> P (n + 1) env={n=2}) □ (ch -> P (n + 1) env={n=2}))" [label="ch"]
  "((ch -> P (n + 1) env={n=1}) □ (ch -> P (n + 1) env={n=1}))" -> "((ch -> P (n + 1) env={n=2}) □ (ch -> P (n + 1) env={n=2}))" [label="ch"]
  "((ch -> P (n + 1) env={n=0}) □ (ch -> P (n + 1) env={n=0}))" -> "((ch -> P (n + 1) env={n=1}) □ (ch -> P (n + 1) env={n=1}))" [label="ch"]
  "((ch -> P (n + 1) env={n=0}) □ (ch -> P (n + 1) env={n=0}))" -> "((ch -> P (n + 1) env={n=1}) □ (ch -> P (n + 1) env={n=1}))" [label="ch"]
}""" = actual, actual)
