module CSP.Core.Tests

open Xunit
open CSP.Core.Proc

[<Fact>]
let abSkip () =
    let m =
        Map
            [ ("ABskip", Seq(Unwind "Askip", Unwind "Bskip"))
              ("Askip", Prefix("a", Skip))
              ("Bskip", Prefix("b", Skip)) ]

    let env = Map.empty in
    let actual = dot m env "ABskip" in

    Assert.True(
        """digraph G {
  "(Askip ; Bskip)" -> "(SKIP ; Bskip)" [label="Vis 'a'"]
  "(SKIP ; Bskip)" -> "(b -> SKIP)" [label="Tau"]
  "(b -> SKIP)" -> "SKIP" [label="Vis 'b'"]
  "SKIP" -> "Ω" [label="Tick"]
}""" =
            actual,
        actual
    )

[<Fact>]
let parABC () =
    let m =
        Map
            [ ("ParABC",
               InterfaceParallel(
                   Prefix("a", Skip),
                   Set.empty,
                   InterfaceParallel(Prefix("b", Skip), Set.empty, Prefix("c", Skip))
               ))
              ("P", Seq(Unwind "ParABC", Prefix("d", Skip))) ] in

    let env = Map.empty in
    let actual = dot m env "P" in

    Assert.True(
        """digraph G {
  "(ParABC ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Vis 'b'"]
  "(ParABC ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "(ParABC ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Vis 'a'"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Vis 'b'"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Tau"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Vis 'b'"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'b'"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Vis 'b'"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ Ω) ; (d -> SKIP))" [label="Tau"]
  "((Ω ⟦{}⟧ Ω) ; (d -> SKIP))" -> "(d -> SKIP)" [label="Tau"]
  "(d -> SKIP)" -> "SKIP" [label="Vis 'd'"]
  "SKIP" -> "Ω" [label="Tick"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Tau"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Tau"]
  "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'b'"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Vis 'b'"]
  "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ Ω) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ Ω) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ Ω) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Tau"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((Ω ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Tau"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'b'"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'a'"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Vis 'b'"]
  "(((a -> SKIP) ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ ((b -> SKIP) ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Vis 'a'"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Vis 'a'"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ Ω) ; (d -> SKIP))" [label="Tau"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Vis 'a'"]
  "(((a -> SKIP) ⟦{}⟧ Ω) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ Ω) ; (d -> SKIP))" [label="Vis 'a'"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Tau"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'a'"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω)) ; (d -> SKIP))" [label="Tau"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'a'"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Tau"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "(((a -> SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Vis 'a'"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP)) ; (d -> SKIP))" [label="Vis 'c'"]
  "(((a -> SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" -> "((SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c -> SKIP))) ; (d -> SKIP))" [label="Vis 'a'"]
}""" =
            actual,
        actual
    )


[<Fact>]
let rand2 () =
    let m = Map [ ("P", IntCh(Prefix(1, Unwind "P"), Prefix(2, Unwind "P"))) ] in
    let env = Map.empty in
    let actual = dot m env "P" in

    Assert.True(
        """digraph G {
  "((1 -> P) ⨅ (2 -> P))" -> "(2 -> P)" [label="Tau"]
  "((1 -> P) ⨅ (2 -> P))" -> "(1 -> P)" [label="Tau"]
  "(1 -> P)" -> "((1 -> P) ⨅ (2 -> P))" [label="Vis 1"]
  "(2 -> P)" -> "((1 -> P) ⨅ (2 -> P))" [label="Vis 2"]
}""" =
            actual,
        actual
    )

[<Fact>]
let abs () =
    let m =
        Map [ ("ABS", ExtCh(IntCh(Prefix("a", Unwind "ABS"), Prefix("b", Unwind "ABS")), Prefix("s", Stop))) ] in

    let env = Map.empty in
    let actual = dot m env "ABS" in

    Assert.True(
        """digraph G {
  "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" -> "((a -> ABS) □ (s -> STOP))" [label="Tau"]
  "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" -> "((b -> ABS) □ (s -> STOP))" [label="Tau"]
  "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" -> "STOP" [label="Vis 's'"]
  "((b -> ABS) □ (s -> STOP))" -> "STOP" [label="Vis 's'"]
  "((b -> ABS) □ (s -> STOP))" -> "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" [label="Vis 'b'"]
  "((a -> ABS) □ (s -> STOP))" -> "STOP" [label="Vis 's'"]
  "((a -> ABS) □ (s -> STOP))" -> "(((a -> ABS) ⨅ (b -> ABS)) □ (s -> STOP))" [label="Vis 'a'"]
}""" =
            actual,
        actual
    )

[<Fact>]
let lr () =
    let m =
        Map
            [ ("LR", InterfaceParallel(Unwind "Left", Set [ "sync" ], Unwind "Right"))
              ("Left", Prefix("blue", Prefix("sync", Unwind "Left")))
              ("Right", Prefix("red", Prefix("sync", Unwind "Right"))) ] in

    let env = Map.empty in
    let actual = dot m env "LR" in

    Assert.True(
        """digraph G {
  "(Left ⟦{sync}⟧ Right)" -> "(Left ⟦{sync}⟧ (sync -> Right))" [label="Vis 'red'"]
  "(Left ⟦{sync}⟧ Right)" -> "((sync -> Left) ⟦{sync}⟧ Right)" [label="Vis 'blue'"]
  "((sync -> Left) ⟦{sync}⟧ Right)" -> "((sync -> Left) ⟦{sync}⟧ (sync -> Right))" [label="Vis 'red'"]
  "((sync -> Left) ⟦{sync}⟧ (sync -> Right))" -> "(Left ⟦{sync}⟧ Right)" [label="Vis 'sync'"]
  "(Left ⟦{sync}⟧ (sync -> Right))" -> "((sync -> Left) ⟦{sync}⟧ (sync -> Right))" [label="Vis 'blue'"]
}""" =
            actual,
        actual
    )

[<Fact>]
let coinToss () =
    let m =
        Map
            [ ("Coin", Prefix("toss", Unwind "Coin'"))
              ("Coin'", IntCh(Prefix("heads", Unwind "Coin"), Prefix("tails", Unwind "Coin")))
              ("Man", Prefix("toss", Unwind "Man'"))
              ("Man'",
               ExtCh(Prefix("heads", Prefix("left", Unwind "Man")), Prefix("tails", Prefix("right", Unwind "Man"))))
              ("CoinToss", InterfaceParallel(Unwind "Coin", Set [ "toss"; "heads"; "tails" ], Unwind "Man")) ]

    let env = Map.empty in
    let actual = dot m env "CoinToss" in

    Assert.True(
        """digraph G {
  "(Coin ⟦{heads, tails, toss}⟧ Man)" -> "(Coin' ⟦{heads, tails, toss}⟧ Man')" [label="Vis 'toss'"]
  "(Coin' ⟦{heads, tails, toss}⟧ Man')" -> "((heads -> Coin) ⟦{heads, tails, toss}⟧ Man')" [label="Tau"]
  "(Coin' ⟦{heads, tails, toss}⟧ Man')" -> "((tails -> Coin) ⟦{heads, tails, toss}⟧ Man')" [label="Tau"]
  "((tails -> Coin) ⟦{heads, tails, toss}⟧ Man')" -> "(Coin ⟦{heads, tails, toss}⟧ (right -> Man))" [label="Vis 'tails'"]
  "(Coin ⟦{heads, tails, toss}⟧ (right -> Man))" -> "(Coin ⟦{heads, tails, toss}⟧ Man)" [label="Vis 'right'"]
  "((heads -> Coin) ⟦{heads, tails, toss}⟧ Man')" -> "(Coin ⟦{heads, tails, toss}⟧ (left -> Man))" [label="Vis 'heads'"]
  "(Coin ⟦{heads, tails, toss}⟧ (left -> Man))" -> "(Coin ⟦{heads, tails, toss}⟧ Man)" [label="Vis 'left'"]
}""" =
            actual,
        actual
    )

[<Fact>]
let lrh () =
    let m =
        Map
            [ ("LRH", Hide(InterfaceParallel(Unwind "Left", Set [ "sync" ], Unwind "Right"), Set [ "sync" ]))
              ("Left", Prefix("blue", Prefix("sync", Unwind "Left")))
              ("Right", Prefix("red", Prefix("sync", Unwind "Right"))) ] in

    let env = Map.empty in
    let actual = dot m env "LRH" in

    Assert.True(
        """digraph G {
  "((Left ⟦{sync}⟧ Right) \\ sync)" -> "((Left ⟦{sync}⟧ (sync -> Right)) \\ sync)" [label="Vis 'red'"]
  "((Left ⟦{sync}⟧ Right) \\ sync)" -> "(((sync -> Left) ⟦{sync}⟧ Right) \\ sync)" [label="Vis 'blue'"]
  "(((sync -> Left) ⟦{sync}⟧ Right) \\ sync)" -> "(((sync -> Left) ⟦{sync}⟧ (sync -> Right)) \\ sync)" [label="Vis 'red'"]
  "(((sync -> Left) ⟦{sync}⟧ (sync -> Right)) \\ sync)" -> "((Left ⟦{sync}⟧ Right) \\ sync)" [label="Tau"]
  "((Left ⟦{sync}⟧ (sync -> Right)) \\ sync)" -> "(((sync -> Left) ⟦{sync}⟧ (sync -> Right)) \\ sync)" [label="Vis 'blue'"]
}""" =
            actual,
        actual
    )

[<Fact>]
let hide3 () =
    let m = Map [ ("P", Hide(Prefix("a", Skip), Set [ "a" ])) ] in
    let env = Map.empty in
    let actual = dot m env "P" in
    Assert.True("""""" = actual, actual)
