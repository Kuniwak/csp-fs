module CSP.Core.Tests.GraphTests

open CSP.Core
open CSP.Core.Util
open Xunit
open CSP.Model
open CSP.Model.Examples
open CSP.Core.ProcEval
open CSP.Core.Trans
open CSP.Core.Search
open CSP.Core.Eval
open CSP.Core.ValShorthand
open CSP.Core.ProcEvalError
open CSP.Core.Univ
open CSP.Core.Visualization.DotLang

let univCfg: UnivConfig = { NatMax = 5u; ListLenMax = 3u }

let procEvalCfg: ProcEvalConfig = { EvalConfig = { UnivConfig = univCfg } }

let dotCfg: DotConfig =
    { GraphConfig =
        { TransConfig = { ProcEvalConfig = procEvalCfg }
          ProcEvalConfig = procEvalCfg
          SearchConfig = { NodeMax = 1000 }
          NamedConfig =
            { ProcEvalConfig = procEvalCfg
              UnivConfig = univCfg } } }

let dot pm um =
    um |> CtorMap.from |> ResultEx.get CtorMapError.format |> dot dotCfg pm um

[<Fact>]
let abSkip () =
    match dot ABSkip.procMap ABSkip.unionMap ABSkip.genv "ABSkip" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->
        Assert.True(
            """digraph G {
  "Ω"
  "SKIP"
  "ABSkip"

  "SKIP" -> "Ω" [label="✓"]
  "ABSkip" -> "SKIP" [label="a"]
}""" =
                actual,
            actual
        )

[<Fact>]
let parABC () =
    match dot ParABC.procMap ParABC.unionMap ParABC.genv "P" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "Ω"
  "(Ω ⟦{}⟧ Ω)"
  "(SKIP ⟦{}⟧ Ω)"
  "(Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω))"
  "((a → SKIP) ⟦{}⟧ Ω)"
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω))"
  "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω))"
  "(Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP))"
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω))"
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω))"
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP))"
  "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))"
  "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))"
  "(Ω ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))"
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω))"
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP))"
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))"
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))"
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))"
  "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))"
  "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))"
  "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))"
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))"
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))"
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))"
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))"
  "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))"
  "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))"
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))"
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))"
  "P"

  "(Ω ⟦{}⟧ Ω)" -> "Ω" [label="✓"]
  "(SKIP ⟦{}⟧ Ω)" -> "(Ω ⟦{}⟧ Ω)" [label="τ"]
  "(Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω))" -> "(Ω ⟦{}⟧ Ω)" [label="τ"]
  "((a → SKIP) ⟦{}⟧ Ω)" -> "(SKIP ⟦{}⟧ Ω)" [label="a"]
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω))" -> "(Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω))" [label="τ"]
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω))" -> "(SKIP ⟦{}⟧ Ω)" [label="τ"]
  "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" -> "(Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω))" [label="τ"]
  "(Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" -> "(Ω ⟦{}⟧ (Ω ⟦{}⟧ Ω))" [label="τ"]
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω))" -> "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω))" [label="a"]
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω))" -> "((a → SKIP) ⟦{}⟧ Ω)" [label="τ"]
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" -> "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" [label="τ"]
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" -> "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω))" [label="τ"]
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" -> "(Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" [label="τ"]
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" -> "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ Ω))" [label="τ"]
  "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" -> "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" [label="b"]
  "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" -> "(Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" [label="τ"]
  "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" -> "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" [label="τ"]
  "(Ω ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" -> "(Ω ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" [label="c"]
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" -> "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" [label="a"]
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" -> "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω))" [label="τ"]
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" -> "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" [label="a"]
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" -> "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ Ω))" [label="τ"]
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" -> "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" [label="τ"]
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" -> "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" [label="b"]
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" -> "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" [label="τ"]
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" -> "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" [label="τ"]
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" -> "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" [label="τ"]
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" -> "(Ω ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" [label="τ"]
  "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" -> "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" [label="c"]
  "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" -> "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" [label="b"]
  "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" -> "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" [label="τ"]
  "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" -> "(Ω ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" [label="τ"]
  "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" -> "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" [label="c"]
  "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" -> "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" [label="a"]
  "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" -> "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" [label="b"]
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" -> "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" [label="a"]
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" -> "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" [label="τ"]
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" -> "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ Ω))" [label="τ"]
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" -> "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" [label="a"]
  "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" -> "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ SKIP))" [label="c"]
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" -> "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" [label="τ"]
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" -> "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" [label="b"]
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" -> "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" [label="τ"]
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" -> "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" [label="τ"]
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" -> "(SKIP ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" [label="τ"]
  "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" -> "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" [label="c"]
  "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))" -> "(Ω ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" [label="b"]
  "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))" -> "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" [label="c"]
  "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" -> "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" [label="a"]
  "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" -> "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" [label="b"]
  "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" -> "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ Ω))" [label="τ"]
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" -> "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" [label="a"]
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" -> "((a → SKIP) ⟦{}⟧ (Ω ⟦{}⟧ (c → SKIP)))" [label="τ"]
  "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" -> "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ SKIP))" [label="c"]
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))" -> "(Ω ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))" [label="τ"]
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))" -> "(SKIP ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" [label="b"]
  "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))" -> "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" [label="c"]
  "P" -> "(SKIP ⟦{}⟧ ((b → SKIP) ⟦{}⟧ (c → SKIP)))" [label="a"]
  "P" -> "((a → SKIP) ⟦{}⟧ (SKIP ⟦{}⟧ (c → SKIP)))" [label="b"]
  "P" -> "((a → SKIP) ⟦{}⟧ ((b → SKIP) ⟦{}⟧ SKIP))" [label="c"]
}""" =
                actual,
            actual
        )

[<Fact>]
let rand2 () =
    match dot Rand2.procMap Rand2.unionMap Rand2.genv "P" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->
        Assert.True(
            """digraph G {
  "(2 → P)"
  "(1 → P)"
  "P"

  "(2 → P)" -> "P" [label="2"]
  "(1 → P)" -> "P" [label="1"]
  "P" -> "(1 → P)" [label="τ"]
  "P" -> "(2 → P)" [label="τ"]
}""" =
                actual,
            actual
        )

[<Fact>]
let abs () =
    match dot ABS.procMap ABS.unionMap ABS.genv "ABS" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->
        Assert.True(
            """digraph G {
  "STOP"  [fillcolor=red, style=filled, fontcolor=white]
  "((b → ABS) □ (s → STOP))"
  "((a → ABS) □ (s → STOP))"
  "ABS"

  "((b → ABS) □ (s → STOP))" -> "ABS" [label="b"]
  "((b → ABS) □ (s → STOP))" -> "STOP" [label="s"]
  "((a → ABS) □ (s → STOP))" -> "ABS" [label="a"]
  "((a → ABS) □ (s → STOP))" -> "STOP" [label="s"]
  "ABS" -> "((a → ABS) □ (s → STOP))" [label="τ"]
  "ABS" -> "((b → ABS) □ (s → STOP))" [label="τ"]
  "ABS" -> "STOP" [label="s"]
}""" =
                actual,
            actual
        )

[<Fact>]
let lr () =
    match dot LR.procMap LR.unionMap LR.genv "LR" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "((sync → Left) ⟦{sync}⟧ (sync → Right))"
  "(Left ⟦{sync}⟧ (sync → Right))"
  "((sync → Left) ⟦{sync}⟧ Right)"
  "LR"

  "((sync → Left) ⟦{sync}⟧ (sync → Right))" -> "LR" [label="sync"]
  "(Left ⟦{sync}⟧ (sync → Right))" -> "((sync → Left) ⟦{sync}⟧ (sync → Right))" [label="blue"]
  "((sync → Left) ⟦{sync}⟧ Right)" -> "((sync → Left) ⟦{sync}⟧ (sync → Right))" [label="red"]
  "LR" -> "((sync → Left) ⟦{sync}⟧ Right)" [label="blue"]
  "LR" -> "(Left ⟦{sync}⟧ (sync → Right))" [label="red"]
}""" =
                actual,
            actual
        )

[<Fact>]
let coinToss () =
    match dot CoinToss.procMap CoinToss.unionMap CoinToss.genv "CoinToss" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "(Coin ⟦{heads, tails, toss}⟧ (right → Man))"
  "(Coin ⟦{heads, tails, toss}⟧ (left → Man))"
  "((tails → Coin) ⟦{heads, tails, toss}⟧ Man')"
  "((heads → Coin) ⟦{heads, tails, toss}⟧ Man')"
  "(Coin' ⟦{heads, tails, toss}⟧ Man')"
  "CoinToss"

  "(Coin ⟦{heads, tails, toss}⟧ (right → Man))" -> "CoinToss" [label="right"]
  "(Coin ⟦{heads, tails, toss}⟧ (left → Man))" -> "CoinToss" [label="left"]
  "((tails → Coin) ⟦{heads, tails, toss}⟧ Man')" -> "(Coin ⟦{heads, tails, toss}⟧ (right → Man))" [label="tails"]
  "((heads → Coin) ⟦{heads, tails, toss}⟧ Man')" -> "(Coin ⟦{heads, tails, toss}⟧ (left → Man))" [label="heads"]
  "(Coin' ⟦{heads, tails, toss}⟧ Man')" -> "((heads → Coin) ⟦{heads, tails, toss}⟧ Man')" [label="τ"]
  "(Coin' ⟦{heads, tails, toss}⟧ Man')" -> "((tails → Coin) ⟦{heads, tails, toss}⟧ Man')" [label="τ"]
  "CoinToss" -> "(Coin' ⟦{heads, tails, toss}⟧ Man')" [label="toss"]
}""" =
                actual,
            actual
        )

[<Fact>]
let lrh () =
    match dot LRH.procMap LRH.unionMap LRH.genv "LRH" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "(((sync → Left) ⟦{sync}⟧ (sync → Right)) \\ {sync})"
  "((Left ⟦{sync}⟧ (sync → Right)) \\ {sync})"
  "(((sync → Left) ⟦{sync}⟧ Right) \\ {sync})"
  "LRH"

  "(((sync → Left) ⟦{sync}⟧ (sync → Right)) \\ {sync})" -> "LRH" [label="τ (sync)"]
  "((Left ⟦{sync}⟧ (sync → Right)) \\ {sync})" -> "(((sync → Left) ⟦{sync}⟧ (sync → Right)) \\ {sync})" [label="blue"]
  "(((sync → Left) ⟦{sync}⟧ Right) \\ {sync})" -> "(((sync → Left) ⟦{sync}⟧ (sync → Right)) \\ {sync})" [label="red"]
  "LRH" -> "(((sync → Left) ⟦{sync}⟧ Right) \\ {sync})" [label="blue"]
  "LRH" -> "((Left ⟦{sync}⟧ (sync → Right)) \\ {sync})" [label="red"]
}""" =
                actual,
            actual
        )

[<Fact>]
let hide3 () =
    match dot Hide3.procMap Hide3.unionMap Hide3.genv "P" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "Ω"
  "(SKIP \\ {a})"
  "P"

  "(SKIP \\ {a})" -> "Ω" [label="✓"]
  "P" -> "(SKIP \\ {a})" [label="τ (a)"]
}""" =
                actual,
            actual
        )

[<Fact>]
let count () =
    match dot Count.procMap Count.unionMap Count.genv "COUNT" [ vNat 0u ] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "(COUNT 10)"
  "(COUNT 9)"
  "(COUNT 8)"
  "(COUNT 7)"
  "(COUNT 6)"
  "(COUNT 5)"
  "(COUNT 4)"
  "(COUNT 3)"
  "(COUNT 2)"
  "(COUNT 1)"
  "(COUNT 0)"

  "(COUNT 10)" -> "(COUNT 0)" [label="reset"]
  "(COUNT 9)" -> "(COUNT 10)" [label="push"]
  "(COUNT 8)" -> "(COUNT 9)" [label="push"]
  "(COUNT 7)" -> "(COUNT 8)" [label="push"]
  "(COUNT 6)" -> "(COUNT 7)" [label="push"]
  "(COUNT 5)" -> "(COUNT 6)" [label="push"]
  "(COUNT 4)" -> "(COUNT 5)" [label="push"]
  "(COUNT 3)" -> "(COUNT 4)" [label="push"]
  "(COUNT 2)" -> "(COUNT 3)" [label="push"]
  "(COUNT 1)" -> "(COUNT 2)" [label="push"]
  "(COUNT 0)" -> "(COUNT 1)" [label="push"]
}""" =
                actual,
            actual
        )

[<Fact>]
let roVarSys1 () =
    match dot ROVarSys1.procMap ROVarSys1.unionMap ROVarSys1.genv "ROVarSys1" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{0, 1, 2, 3, 4, 5}⟧ STOP)))"  [fillcolor=red, style=filled, fontcolor=white]
  "ROVarSys1"

  "ROVarSys1" -> "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{0, 1, 2, 3, 4, 5}⟧ STOP)))" [label="0"]
}""" =
                actual,
            actual
        )

[<Fact>]
let roVarSys2 () =
    match dot ROVarSys2.procMap ROVarSys2.unionMap ROVarSys2.genv "ROVarSys2" [] with
    | Error(err) -> Assert.Fail(format err)
    | Ok(actual) ->

        Assert.True(
            """digraph G {
  "((ROVar 3) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP)))"  [fillcolor=red, style=filled, fontcolor=white]
  "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP)))"
  "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))"
  "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))"
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))"
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))"
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))"
  "ROVarSys2"

  "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" -> "((ROVar 3) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="2"]
  "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" -> "((ROVar 3) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="2"]
  "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" -> "((ROVar 3) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="2"]
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" -> "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" [label="1"]
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" -> "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="1"]
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" -> "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" [label="1"]
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" -> "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ STOP)))" [label="1"]
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" -> "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" [label="1"]
  "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" -> "((ROVar 2) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" [label="1"]
  "ROVarSys2" -> "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (STOP ⟦{}⟧ (Reader2 ⟦{}⟧ Reader3)))" [label="0"]
  "ROVarSys2" -> "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (STOP ⟦{}⟧ Reader3)))" [label="0"]
  "ROVarSys2" -> "((ROVar 1) ⟦{0, 1, 2, 3, 4, 5}⟧ (Reader1 ⟦{}⟧ (Reader2 ⟦{}⟧ STOP)))" [label="0"]
}""" =
                actual,
            actual
        )
