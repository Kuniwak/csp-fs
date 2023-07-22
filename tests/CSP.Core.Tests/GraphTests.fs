module CSP.Core.Tests.GraphTests

open Xunit
open CSP.Core
open CSP.Core.Search
open CSP.Core.TypeShorthand
open CSP.Core.Var
open CSP.Core.Val
open CSP.Core.ExprShorthand
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.Eval
open CSP.Core.Graph

let evalCfg: EvalConfig = { UnivConfig = { NatMax = 5u; ListLenMax = 3u } }
let dotCfg = dotConfig (searchConfig 100) evalCfg
let dot = dot dotCfg

[<Fact>]
let abSkip () =
    let tEvent = tUnion "event" [ ("a", []); ("b", []) ]

    let pm =
        from
            [ ("ABSkip", None), seq (unwind "ASkip" None) (unwind "BSkip" None)
              ("ASkip", None), prefix (ctor "a" []) skip
              ("BSkip", None), prefix (ctor "b" []) skip ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "ABSkip" None in

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
            [ (("ParABC", None),
               interleave
                   (prefix (ctor "a" []) skip)
                   (interleave (prefix (ctor "b" []) skip) (prefix (ctor "c" []) skip)))
              (("P", None), seq (unwind "ParABC" None) (prefix (ctor "d" []) skip)) ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "P" None in

    Assert.True(
        """""" =
            actual,
        actual
    )

[<Fact>]
let rand2 () =
    let pm =
        from [ (("P", None), intCh (prefix (litNat 1u) (unwind "P" None)) (prefix (litNat 2u) (unwind "P" None))) ] in

    let cm = CtorMap.empty
    let genv = Env.empty in
    let actual = dot pm cm genv "P" None in

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
            [ (("ABS", None),
               (extCh
                   (intCh (prefix (ctor "a" []) (unwind "ABS" None)) (prefix (ctor "b" []) (unwind "ABS" None)))
                   (prefix (ctor "s" []) stop))) ] in

    let cm = CtorMap.from [ tEvent ]
    let genv = Env.empty in
    let actual = dot pm cm genv "ABS" None in

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
    let tEvent = tUnion "event" [ ("blue", []); ("red", []); ("sync", []) ]

    let pm =
        from
            [ (("LR", None),
               (interfaceParallel
                   (unwind "Left" None)
                   (setInsert (ctor "sync" []) (litEmpty (tSet tEvent)))
                   (unwind "Right" None)))
              (("Left", None), prefix (ctor "blue" []) (prefix (ctor "sync" []) (unwind "Left" None)))
              (("Right", None), prefix (ctor "red" []) (prefix (ctor "sync" []) (unwind "Right" None))) ] in

    let cm = CtorMap.from [ tEvent ] in

    let genv = Env.empty in
    let actual = dot pm cm genv "LR" None in

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
}
""" =
            actual,
        actual
    )

[<Fact>]
let coinToss () =
    let tEvent =
        tUnion "event" [ ("toss", []); ("heads", []); ("tails", []); ("right", []); ("left", []) ]

    let pm =
        from
            [ (("Coin", None), prefix (ctor "toss" []) (unwind "Coin'" None))
              (("Coin'", None),
               intCh (prefix (ctor "heads" []) (unwind "Coin" None)) (prefix (ctor "tails" []) (unwind "Coin" None)))
              (("Man", None), prefix (ctor "toss" []) (unwind "Man'" None))
              (("Man'", None),
               extCh
                   (prefix (ctor "heads" []) (prefix (ctor "left" []) (unwind "Man" None)))
                   (prefix (ctor "tails" []) (prefix (ctor "right" []) (unwind "Man" None))))
              (("CoinToss", None),
               interfaceParallel
                   (unwind "Coin" None)
                   (setInsert
                       (ctor "toss" [])
                       (setInsert (ctor "heads" []) (setInsert (ctor "tails" []) (litEmpty (tSet tEvent)))))
                   (unwind "Man" None)) ]

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "CoinToss" None in

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
            [ (("LRH", None),
               hide
                   (interfaceParallel
                       (unwind "Left" None)
                       (setInsert (ctor "sync" []) (litEmpty (tSet tEvent)))
                       (unwind "Right" None))
                   (setInsert (ctor "sync" []) (litEmpty (tSet tEvent))))
              (("Left", None), prefix (ctor "blue" []) (prefix (ctor "sync" []) (unwind "Left" None)))
              (("Right", None), prefix (ctor "red" []) (prefix (ctor "sync" []) (unwind "Right" None))) ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "LRH" None in

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
        from [ ("P", None), hide (prefix (ctor "a" []) skip) (setInsert (ctor "a" []) (litEmpty (tSet tEvent))) ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "P" None in

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
            [ (("COUNT", Some(Var "n")),
               extCh
                   (guard
                       (less tNat (varRef "n") (litNat 10u))
                       (prefix (ctor "push" []) (unwind "COUNT" (Some(plus tNat (varRef "n") (litNat 1u))))))
                   (guard
                       (eq tNat (varRef "n") (litNat 10u))
                       (prefix (ctor "reset" []) (unwind "COUNT" (Some(litNat 0u)))))) ] in

    let cm = CtorMap.from [ tEvent ] in
    let genv = Env.empty in
    let actual = dot pm cm genv "COUNT" (Some(VNat 0u)) in

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
    let tReadCh = tUnion "read" [ ("Read", [ tNat ]) ]

    let pm =
        from
            [ (("ROVarSys1", None),
               interfaceParallel
                   (unwind "ROVar" (Some(litNat 0u)))
                   (univ tReadCh)
                   (interfaceParallel
                       (unwind "Reader1" None)
                       (univ tReadCh)
                       (interfaceParallel (unwind "Reader2" None) (univ tReadCh) (unwind "Reader3" None))))
              (("ROVar", Some(Var "x")),
               prefix
                   (ctor "Read" [ varRef "x" ])
                   (unwind
                       "ROVar"
                       (Some(
                           ifExpr (less tNat (varRef "x") (litNat 4u)) (plus tNat (varRef "x") (litNat 1u)) (litNat 0u)
                       ))))
              (("Reader1", None), prefixRecv (univ tReadCh) "x" stop)
              (("Reader2", None), prefixRecv (univ tReadCh) "x" stop)
              (("Reader3", None), prefixRecv (univ tReadCh) "x" stop) ] in

    let cm = CtorMap.from [ tReadCh ]
    let genv = Env.empty in
    let actual = dot pm cm genv "ROVarSys1" None in

    Assert.True(
        """digraph G {
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=4} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=3} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})"
  "(ROVar 0 env={} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})"
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=4} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" [label="(Read 0)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=3} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=4} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" [label="(Read 4)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=3} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" [label="(Read 3)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=2} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" [label="(Read 2)"]
  "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=1} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" [label="(Read 1)"]
  "(ROVar 0 env={} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" -> "(ROVar (if (nat.less x 4) then (nat.plus x 1) else 0) env={x=0} ⟦(univ::(read Read [nat]))⟧ (Reader1 ⟦(univ::(read Read [nat]))⟧ (Reader2 ⟦(univ::(read Read [nat]))⟧ Reader3 env={}) env={}) env={})" [label="(Read 0)"]
}""" =
            actual,
        actual
    )

[<Fact>]
let roVarSys2 () =
    let tReadCh = tUnion "read" [ ("Read", [ tNat ]) ]

    let pm =
        from
            [ (("ROVarSys2", None),
               (interfaceParallel
                   (unwind "ROVar" (Some(litNat 0u)))
                   (univ tReadCh)
                   (interleave (unwind "Reader1" None) (interleave (unwind "Reader2" None) (unwind "Reader3" None)))))
              (("ROVar", Some(Var "x")),
               (prefix
                   (ctor "Read" [ varRef "x" ])
                   (unwind
                       "ROVar"
                       (Some(
                           ifExpr (less tNat (varRef "x") (litNat 4u)) (plus tNat (varRef "x") (litNat 1u)) (litNat 0u)
                       )))))
              (("Reader1", None), prefixRecv (univ tReadCh) "x" stop)
              (("Reader2", None), prefixRecv (univ tReadCh) "x" stop)
              (("Reader3", None), prefixRecv (univ tReadCh) "x" stop) ] in

    let cm = CtorMap.from [ tReadCh ]
    let genv = Env.empty in
    let actual = dot pm cm genv "ROVarSys2" None in

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
