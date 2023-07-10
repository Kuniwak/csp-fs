F# による CSP インタプリタ
==========================

[Communicating Sequential Proccesses (CSP)](https://ja.wikipedia.org/wiki/Communicating_Sequential_Processes) の F# 処理系です。

[DOT言語](https://ja.wikipedia.org/wiki/DOT%E8%A8%80%E8%AA%9E) で状態遷移グラフを表示できます。
コマンドラインでイベントを指定してインタラクティブに実行する機能はまだ未実装です。いずれ実装予定です。



使い方
------

次のように F# でプロセスを定義し、プロセスを入力とする関数を実行してください：

```f#
open CSP.Core.ProcMap
open CSP.Core.Proc
open CSP.Core.Graph

// プロセスを宣言する。
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

// グローバルに参照できる定数を格納した環境を用意する。
let env = Map.empty in

// DOT 言語で記述された状態遷移図を標準出力へ書き込む。
//   max: 探索する状態数の上限。
//   m: プロセスの定義。
//   env: グローバルに参照できる定数を格納した環境。
//   "P": プロセス名。
//   None: プロセスへの引数があれば Some v、なければ None。
printf (dot max m env "P" None)
```



プロセス式
----------

| プロセス（CSP）                            | 書き方                                                                                                                 |
|:-------------------------------------------|:-----------------------------------------------------------------------------------------------------------------------|
| `A`                                        | `Unwind(procName, paramOpt)` （`paramOpt` はプロセスに渡す引数。なければ `None` を、あれば `Some v` のように指定する） |
| `STOP`                                     | `Stop`                                                                                                                 |
| `SKIP`                                     | `Skip`                                                                                                                 |
| `a -> P`                                   | `Prefix(evName, proc)`                                                                                                 |
| `a!x -> P`                                 | `PrefixSend(chName, expr, proc)`                                                                                       |
| `a?x -> P`                                 | `PrefixRecv(chName, varName, type, proc)`                                                                              |
| `P [] Q`                                   | `ExtCh(proc1, proc2)`                                                                                                  |
| `P \|~\| Q`                                | `IntCh(proc1, proc2)`                                                                                                  |
| `if c then P else Q`                       | `If(expr, procTrue, procFalse)`                                                                                        |
| `match v with X -> P \| Y -> Q \| _ -> R`  | `Match(expr, Map [(ctor1, (var1, proc1)); (ctor2, (var2, proc2)); ...], Some (varOther, procOther))`                   |
| `P; Q`                                     | `Seq(proc1, proc2)`                                                                                                    |
| `P [\| X \|] Q`                            | `InterfaceParallel(proc1, evSpecs, proc2)` (`evSpecs` はイベント指定子の集合)                                          |
| `P \\ X`                                   | `Hide(proc, evSpecs)` (`evSpecs` はイベント指定子の集合)                                                               |
| `P[[a ←b]]`                               | 未実装                                                                                                                 |
| `[] x:S @ P(x)`                            | 実装予定                                                                                                               |
| `\|~\| x:S @ P(x)`                             | 実装予定                                                                                                               |
| `[\|X\|] x:S @ P(x)`                       | 実装予定                                                                                                               |
| `P \|\|\| Q`                               | `Interleave(proc1, proc2)`                                                                                             |
| `\|\|\| x:S @ P(x)`                        | 実装予定                                                                                                               |



式
--

真偽値、自然数、集合、列、辞書に対する一部の演算を表現できます。量が多いので [`Expr.fs`](./src/CSP.Core/Expr.fs) を参照してください。



イベント指定子
--------------

イベントまたはチャンネルを指定するための構文です。

| イベント指定子 | 書き方                                                       |
|:---------------|:-------------------------------------------------------------|
| イベント       | `Event ev`                                                   |
| チャンネル     | `Chan ch` （送受信される値を特定する記法は実装していません） |
