module CSP.Core.Tests

open Xunit
open CSP.Core.Val
open CSP.Core.Expr
open CSP.Core.ProcMap
open CSP.Core.State
open CSP.Core.Proc

[<Fact>]
let abSkip () =
    let m: Map<string, Unit option * Proc<string, string, Unit, Unit, Unit>> =
        Map
            [ ("ABskip", (None, Seq(Unwind("Askip", None), Unwind("Bskip", None))))
              ("Askip", (None, Prefix("a", Skip)))
              ("Bskip", (None, Prefix("b", Skip))) ]

    let env = Map.empty in
    let actual = dot m env "ABskip" None in

    Assert.True(
        """digraph G {
  "((Askip) ; (Bskip))" -> "((SKIP) ; (Bskip))" [label="a"]
  "((SKIP) ; (Bskip))" -> "(b -> (SKIP))" [label="τ"]
  "(b -> (SKIP))" -> "(SKIP)" [label="b"]
  "(SKIP)" -> "(Ω)" [label="✓"]
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
    let actual = dot m env "P" None in

    Assert.True(
        """digraph G {
  "((ParABC) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="a"]
  "((ParABC) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "((ParABC) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="b"]
  "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="a"]
  "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="τ"]
  "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="a"]
  "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="a"]
  "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="a"]
  "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ (Ω)) ; (d -> (SKIP)))" [label="τ"]
  "(((a -> (SKIP)) ⟦{}⟧ (Ω)) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ (Ω)) ; (d -> (SKIP)))" [label="a"]
  "(((SKIP) ⟦{}⟧ (Ω)) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ (Ω)) ; (d -> (SKIP)))" [label="τ"]
  "(((Ω) ⟦{}⟧ (Ω)) ; (d -> (SKIP)))" -> "(d -> (SKIP))" [label="τ"]
  "(d -> (SKIP))" -> "(SKIP)" [label="d"]
  "(SKIP)" -> "(Ω)" [label="✓"]
  "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ (Ω)) ; (d -> (SKIP)))" [label="τ"]
  "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ (Ω)) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="a"]
  "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="τ"]
  "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="a"]
  "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="τ"]
  "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="τ"]
  "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((Ω) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="τ"]
  "(((a -> (SKIP)) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="a"]
  "(((a -> (SKIP)) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((a -> (SKIP)) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="b"]
  "(((a -> (SKIP)) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="a"]
  "(((a -> (SKIP)) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((a -> (SKIP)) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="b"]
  "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="b"]
  "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="b"]
  "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="b"]
  "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (Ω))) ; (d -> (SKIP)))" [label="τ"]
  "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="b"]
  "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="τ"]
  "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "(((SKIP) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((SKIP) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="b"]
  "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (SKIP))) ; (d -> (SKIP)))" [label="c"]
  "(((Ω) ⟦{}⟧ ((b -> (SKIP)) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" -> "(((Ω) ⟦{}⟧ ((SKIP) ⟦{}⟧ (c -> (SKIP)))) ; (d -> (SKIP)))" [label="b"]
}""" =
            actual,
        actual
    )


[<Fact>]
let rand2 () =
    let m: ProcMap<string, int, Unit, Unit, Unit> =
        Map [ ("P", (None, IntCh(Prefix(1, Unwind("P", None)), Prefix(2, Unwind("P", None))))) ] in

    let env = Map.empty in
    let actual = dot m env "P" None in

    Assert.True(
        """digraph G {
  "((1 -> (P)) ⨅ (2 -> (P)))" -> "(2 -> (P))" [label="τ"]
  "((1 -> (P)) ⨅ (2 -> (P)))" -> "(1 -> (P))" [label="τ"]
  "(1 -> (P))" -> "((1 -> (P)) ⨅ (2 -> (P)))" [label="1"]
  "(2 -> (P))" -> "((1 -> (P)) ⨅ (2 -> (P)))" [label="2"]
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
    let actual = dot m env "ABS" None in

    Assert.True(
        """digraph G {
  "(((a -> (ABS)) ⨅ (b -> (ABS))) □ (s -> (STOP)))" -> "((a -> (ABS)) □ (s -> (STOP)))" [label="τ"]
  "(((a -> (ABS)) ⨅ (b -> (ABS))) □ (s -> (STOP)))" -> "((b -> (ABS)) □ (s -> (STOP)))" [label="τ"]
  "(((a -> (ABS)) ⨅ (b -> (ABS))) □ (s -> (STOP)))" -> "(STOP)" [label="s"]
  "((b -> (ABS)) □ (s -> (STOP)))" -> "(STOP)" [label="s"]
  "((b -> (ABS)) □ (s -> (STOP)))" -> "(((a -> (ABS)) ⨅ (b -> (ABS))) □ (s -> (STOP)))" [label="b"]
  "((a -> (ABS)) □ (s -> (STOP)))" -> "(STOP)" [label="s"]
  "((a -> (ABS)) □ (s -> (STOP)))" -> "(((a -> (ABS)) ⨅ (b -> (ABS))) □ (s -> (STOP)))" [label="a"]
}""" =
            actual,
        actual
    )

[<Fact>]
let lr () =
    let m: ProcMap<string, string, Unit, Unit, Unit> =
        Map
            [ ("LR",
               (None, InterfaceParallel(Unwind("Left", None), Set [ EventSpec.Event "sync" ], Unwind("Right", None))))
              ("Left", (None, Prefix("blue", Prefix("sync", Unwind("Left", None)))))
              ("Right", (None, Prefix("red", Prefix("sync", Unwind("Right", None))))) ] in

    let env = Map.empty in
    let actual = dot m env "LR" None in

    Assert.True(
        """digraph G {
  "((Left) ⟦{sync}⟧ (Right))" -> "((Left) ⟦{sync}⟧ (sync -> (Right)))" [label="red"]
  "((Left) ⟦{sync}⟧ (Right))" -> "((sync -> (Left)) ⟦{sync}⟧ (Right))" [label="blue"]
  "((sync -> (Left)) ⟦{sync}⟧ (Right))" -> "((sync -> (Left)) ⟦{sync}⟧ (sync -> (Right)))" [label="red"]
  "((sync -> (Left)) ⟦{sync}⟧ (sync -> (Right)))" -> "((Left) ⟦{sync}⟧ (Right))" [label="sync"]
  "((Left) ⟦{sync}⟧ (sync -> (Right)))" -> "((sync -> (Left)) ⟦{sync}⟧ (sync -> (Right)))" [label="blue"]
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
                    Set [ EventSpec.Event "toss"; EventSpec.Event "heads"; EventSpec.Event "tails" ],
                    Unwind("Man", None)
                ))) ]

    let env = Map.empty in
    let actual = dot m env "CoinToss" None in

    Assert.True(
        """digraph G {
  "((Coin) ⟦{heads, tails, toss}⟧ (Man))" -> "((Coin') ⟦{heads, tails, toss}⟧ (Man'))" [label="toss"]
  "((Coin') ⟦{heads, tails, toss}⟧ (Man'))" -> "((heads -> (Coin)) ⟦{heads, tails, toss}⟧ (Man'))" [label="τ"]
  "((Coin') ⟦{heads, tails, toss}⟧ (Man'))" -> "((tails -> (Coin)) ⟦{heads, tails, toss}⟧ (Man'))" [label="τ"]
  "((tails -> (Coin)) ⟦{heads, tails, toss}⟧ (Man'))" -> "((Coin) ⟦{heads, tails, toss}⟧ (right -> (Man)))" [label="tails"]
  "((Coin) ⟦{heads, tails, toss}⟧ (right -> (Man)))" -> "((Coin) ⟦{heads, tails, toss}⟧ (Man))" [label="right"]
  "((heads -> (Coin)) ⟦{heads, tails, toss}⟧ (Man'))" -> "((Coin) ⟦{heads, tails, toss}⟧ (left -> (Man)))" [label="heads"]
  "((Coin) ⟦{heads, tails, toss}⟧ (left -> (Man)))" -> "((Coin) ⟦{heads, tails, toss}⟧ (Man))" [label="left"]
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
                    InterfaceParallel(Unwind("Left", None), Set [ EventSpec.Event "sync" ], Unwind("Right", None)),
                    Set [ EventSpec.Event "sync" ]
                )))
              ("Left", (None, Prefix("blue", Prefix("sync", Unwind("Left", None)))))
              ("Right", (None, Prefix("red", Prefix("sync", Unwind("Right", None))))) ] in

    let env = Map.empty in
    let actual = dot m env "LRH" None in

    Assert.True(
        """digraph G {
  "(((Left) ⟦{sync}⟧ (Right)) \\ {sync})" -> "(((Left) ⟦{sync}⟧ (sync -> (Right))) \\ {sync})" [label="red"]
  "(((Left) ⟦{sync}⟧ (Right)) \\ {sync})" -> "(((sync -> (Left)) ⟦{sync}⟧ (Right)) \\ {sync})" [label="blue"]
  "(((sync -> (Left)) ⟦{sync}⟧ (Right)) \\ {sync})" -> "(((sync -> (Left)) ⟦{sync}⟧ (sync -> (Right))) \\ {sync})" [label="red"]
  "(((sync -> (Left)) ⟦{sync}⟧ (sync -> (Right))) \\ {sync})" -> "(((Left) ⟦{sync}⟧ (Right)) \\ {sync})" [label="τ (sync)"]
  "(((Left) ⟦{sync}⟧ (sync -> (Right))) \\ {sync})" -> "(((sync -> (Left)) ⟦{sync}⟧ (sync -> (Right))) \\ {sync})" [label="blue"]
}""" =
            actual,
        actual
    )

[<Fact>]
let hide3 () =
    let m = Map [ ("P", (None, Hide(Prefix("a", Skip), Set [ EventSpec.Event "a" ]))) ] in
    let env = Map.empty in
    let actual = dot m env "P" None in

    Assert.True(
        """digraph G {
  "((a -> (SKIP)) \\ {a})" -> "((SKIP) \\ {a})" [label="τ (a)"]
  "((SKIP) \\ {a})" -> "(Ω)" [label="✓"]
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
                    Guard(Less ((VarRef "n"), (LitNat 10u)), Prefix("push", Unwind("COUNT", Some (Plus((VarRef "n"), (LitNat 1u)))))),
                    Guard(Eq ((VarRef "n"), (LitNat 10u)), Prefix("reset", Unwind("COUNT", Some (LitNat 0u))))
                ))) ] in

    let env = Map.empty in
    let actual = dot m env "COUNT" (Some (VNat 0u)) in

    Assert.True("""digraph G {
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 0u}) env={n=VNat 0u}) else (STOP env={n=VNat 0u}) env={n=VNat 0u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 0u}) env={n=VNat 0u}) else (STOP env={n=VNat 0u}) env={n=VNat 0u}) env={n=VNat 0u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 1u}) env={n=VNat 1u}) else (STOP env={n=VNat 1u}) env={n=VNat 1u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 1u}) env={n=VNat 1u}) else (STOP env={n=VNat 1u}) env={n=VNat 1u}) env={n=VNat 1u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 1u}) env={n=VNat 1u}) else (STOP env={n=VNat 1u}) env={n=VNat 1u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 1u}) env={n=VNat 1u}) else (STOP env={n=VNat 1u}) env={n=VNat 1u}) env={n=VNat 1u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 2u}) env={n=VNat 2u}) else (STOP env={n=VNat 2u}) env={n=VNat 2u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 2u}) env={n=VNat 2u}) else (STOP env={n=VNat 2u}) env={n=VNat 2u}) env={n=VNat 2u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 2u}) env={n=VNat 2u}) else (STOP env={n=VNat 2u}) env={n=VNat 2u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 2u}) env={n=VNat 2u}) else (STOP env={n=VNat 2u}) env={n=VNat 2u}) env={n=VNat 2u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 3u}) env={n=VNat 3u}) else (STOP env={n=VNat 3u}) env={n=VNat 3u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 3u}) env={n=VNat 3u}) else (STOP env={n=VNat 3u}) env={n=VNat 3u}) env={n=VNat 3u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 3u}) env={n=VNat 3u}) else (STOP env={n=VNat 3u}) env={n=VNat 3u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 3u}) env={n=VNat 3u}) else (STOP env={n=VNat 3u}) env={n=VNat 3u}) env={n=VNat 3u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 4u}) env={n=VNat 4u}) else (STOP env={n=VNat 4u}) env={n=VNat 4u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 4u}) env={n=VNat 4u}) else (STOP env={n=VNat 4u}) env={n=VNat 4u}) env={n=VNat 4u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 4u}) env={n=VNat 4u}) else (STOP env={n=VNat 4u}) env={n=VNat 4u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 4u}) env={n=VNat 4u}) else (STOP env={n=VNat 4u}) env={n=VNat 4u}) env={n=VNat 4u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 5u}) env={n=VNat 5u}) else (STOP env={n=VNat 5u}) env={n=VNat 5u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 5u}) env={n=VNat 5u}) else (STOP env={n=VNat 5u}) env={n=VNat 5u}) env={n=VNat 5u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 5u}) env={n=VNat 5u}) else (STOP env={n=VNat 5u}) env={n=VNat 5u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 5u}) env={n=VNat 5u}) else (STOP env={n=VNat 5u}) env={n=VNat 5u}) env={n=VNat 5u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 6u}) env={n=VNat 6u}) else (STOP env={n=VNat 6u}) env={n=VNat 6u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 6u}) env={n=VNat 6u}) else (STOP env={n=VNat 6u}) env={n=VNat 6u}) env={n=VNat 6u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 6u}) env={n=VNat 6u}) else (STOP env={n=VNat 6u}) env={n=VNat 6u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 6u}) env={n=VNat 6u}) else (STOP env={n=VNat 6u}) env={n=VNat 6u}) env={n=VNat 6u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 7u}) env={n=VNat 7u}) else (STOP env={n=VNat 7u}) env={n=VNat 7u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 7u}) env={n=VNat 7u}) else (STOP env={n=VNat 7u}) env={n=VNat 7u}) env={n=VNat 7u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 7u}) env={n=VNat 7u}) else (STOP env={n=VNat 7u}) env={n=VNat 7u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 7u}) env={n=VNat 7u}) else (STOP env={n=VNat 7u}) env={n=VNat 7u}) env={n=VNat 7u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 8u}) env={n=VNat 8u}) else (STOP env={n=VNat 8u}) env={n=VNat 8u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 8u}) env={n=VNat 8u}) else (STOP env={n=VNat 8u}) env={n=VNat 8u}) env={n=VNat 8u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 8u}) env={n=VNat 8u}) else (STOP env={n=VNat 8u}) env={n=VNat 8u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 8u}) env={n=VNat 8u}) else (STOP env={n=VNat 8u}) env={n=VNat 8u}) env={n=VNat 8u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 9u}) env={n=VNat 9u}) else (STOP env={n=VNat 9u}) env={n=VNat 9u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 9u}) env={n=VNat 9u}) else (STOP env={n=VNat 9u}) env={n=VNat 9u}) env={n=VNat 9u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 9u}) env={n=VNat 9u}) else (STOP env={n=VNat 9u}) env={n=VNat 9u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 9u}) env={n=VNat 9u}) else (STOP env={n=VNat 9u}) env={n=VNat 9u}) env={n=VNat 9u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 10u}) env={n=VNat 10u}) else (STOP env={n=VNat 10u}) env={n=VNat 10u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 10u}) env={n=VNat 10u}) else (STOP env={n=VNat 10u}) env={n=VNat 10u}) env={n=VNat 10u})" [label="push"]
  "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 10u}) env={n=VNat 10u}) else (STOP env={n=VNat 10u}) env={n=VNat 10u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 10u}) env={n=VNat 10u}) else (STOP env={n=VNat 10u}) env={n=VNat 10u}) env={n=VNat 10u})" -> "((if (n < 10) then (push -> (COUNT (n + 1) env={n=VNat 0u}) env={n=VNat 0u}) else (STOP env={n=VNat 0u}) env={n=VNat 0u}) □ (if (n = 10) then (reset -> (COUNT 0 env={n=VNat 0u}) env={n=VNat 0u}) else (STOP env={n=VNat 0u}) env={n=VNat 0u}) env={n=VNat 0u})" [label="reset"]
}""" = actual, actual)
