F# による CSP インタプリタ
==========================

[Communicating Sequential Proccesses (CSP)](https://ja.wikipedia.org/wiki/Communicating_Sequential_Processes) の F# 処理系です。

1. CLI で遷移を選択してプロセスを実行できます
2. [DOT言語](https://ja.wikipedia.org/wiki/DOT%E8%A8%80%E8%AA%9E) で状態遷移グラフを表示できます



構文
----

次のPEG文法で表すS式スタイルの構文をサポートしています：

```peg
Comment ← '"' [^\n]* '\n'
WS ← [\t\r\n ]
Trivia ← (WS / Comment)+

Atom ← [^)\t\r\n ]+ Trivia?
List ←'(' Trivia? (List / Atom)* ')' Trivia?

File ← Trivia? (List / Atom)
```

この構文のトップレベルで許されているデータは次の通りです：

TBD

### インタプリタの利用

次のようにモデル `ParABC.sexp` を定義します：

``sexp
(type event A B C D)

(proc ParABC () 
    (interleave
        (prefix A skip)
        (prefix B skip)
        (prefix C skip)))
(proc P ()
    (seq
        (unwind ParABC)
        (prefix D skip)))
```

```console
$ csp run ./examples/ParABC.sexp '(unwind P)'
state: P
  c 0: D -> SKIP

> c0
state: SKIP
  c 0: ✓ -> Ω

> c0
state: Ω

> q
$ 
```

```console
```}


### 状態遷移図の出力

次のように F# でプロセスを定義し、プロセスを入力とする関数を実行してください：

```f#
open CSP.Core
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.ProcEval
open CSP.Core.Univ
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Visualization.DotLang

// 代数的データ型を宣言しておく。
let tEvent = tUnion "event" [ ("a", []); ("b", []); ("c", []); ("d", []) ]

// 値コンストラクタのテーブルを作成しておく。
let ctorMap = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])

// プロセスを宣言する。
let procMap =
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
               seq (unwind "ParABC" [] __LINE__) (prefix (ctor "d" [] __LINE__) (skip __LINE__) __LINE__) __LINE__) ])

// グローバルに参照できる定数を格納した環境を用意する。
let genv = Env.empty

// 各種設定をする。
let dotCfg: DotConfig =
    { GraphConfig =
        { TransConfig = { ProcEvalConfig = procEvalCfg }
          ProcEvalConfig = procEvalCfg
          SearchConfig = { NodeMax = 10000 }
          NamedConfig =
            { UnivConfig = univCfg
              ProcEvalConfig = procEvalCfg } } }

// DOT 言語で記述された状態遷移図を標準出力へ書き込む。
match dot dotCfg procMap ctorMap genv "ParABC" [] with
| Ok(s) -> printfn $"%s{s}"
| Error(err) -> printfn $"%s{ProcEvalError.format err}"
```



プロセス式
----------

| プロセス（CSP）                            | 書き方                                                                                                                            |
|:-------------------------------------------|:----------------------------------------------------------------------------------------------------------------------------------|
| `A`                                        | `unwind procName exprs __LINE__` （`exprs` はプロセスに渡す引数の式の列）                                                         |
| `STOP`                                     | `stop __LINE__`                                                                                                                   |
| `SKIP`                                     | `skip __LINE__`                                                                                                                   |
| `a -> P`                                   | `prefix expr proc __LINE__`   （チャンネル送信は代数的データ型として評価される `expr` で送る）                                    |
| `a?x -> P`                                 | `prefixRecv expr varName proc __LINE__` （集合として評価される `expr` を渡すとその要素を受信したとき変数 `varName` に束縛される） |
| `P [] Q`                                   | `extCh proc1 proc2 __LINE__`                                                                                                      |
| `P \|~\| Q`                                | `intCh proc1 proc2 __LINE__`                                                                                                      |
| `if c then P else Q`                       | `if expr procTrue procFalse`                                                                                                      |
| `match v with X -> P \| Y -> Q \| _ -> R`  | `match expr [(ctor1, vars1, proc1); (ctor2, (vars2, proc2)); ...; (("_", [defaultVar]), procDefault)] __LINE__`                   |
| `P; Q`                                     | `seq proc1 proc2 __LINE__`                                                                                                        |
| `P [\| X \|] Q`                            | `interfaceParallel proc1 expr proc2 __LINE__` (`expr` はイベントの集合として評価される式)                                         |
| `P \\ X`                                   | `hide proc expr __LINE__` (`expr` はイベントの集合として評価される式)                                                             |
| `c & P`                                    | `guard expr proc __LINE__`                                                                                                        |
| `P[[a ←b]]`                               | 未実装                                                                                                                            |
| `[] x:S @ P(x)`                            | 実装予定                                                                                                                          |
| `\|~\| x:S @ P(x)`                         | 実装予定                                                                                                                          |
| `[\|X\|] x:S @ P(x)`                       | 実装予定                                                                                                                          |
| `P \|\|\| Q`                               | `interleave proc1 proc2 __LINE__`                                                                                                 |
| `\|\|\| x:S @ P(x)`                        | 実装予定                                                                                                                          |



式
--

真偽値、自然数、集合、列、辞書に対する一部の演算を表現できます。量が多いので [`Expr.fs`](./src/CSP.Core/Expr.fs) を参照してください。



チャンネルの送受信
------------------

チャンネルの送受信をするにはデータ構築子をただ1つもつ代数的データ型を定義し、これを`univ`関数に与えて得た値の集合を `prefixRecv` へ与えてください。
