CSP Interpreter by F#
=====================

Implementation for [Communicating Sequential Proccesses (CSP)](https://ja.wikipedia.org/wiki/Communicating_Sequential_Processes).


Syntax
------

Supported S-expression syntax is:

```peg
Comment ← '"' [^\n]* '\n'
WS ← [\t\r\n ]
Trivia ← (WS / Comment)+

Atom ← [^)\t\r\n ]+ Trivia?
List ←'(' Trivia? (List / Atom)* ')' Trivia?

File ← Trivia? (List / Atom)
```

This S-expression must have the following data:

**TBD**


Usage
-----

First, write the file to `ParABC.sexp`:

```sexp
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

To run interpreter, use `csp run`:

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

To visualize, use `csp dot`:

```console
$ csp dot ./examples/ParABC.sexp "(unwind P)"
digraph G {
  "Ω"
  "(Ω ⟦{}⟧ Ω)"
  "((Ω ⟦{}⟧ Ω) ⟦{}⟧ Ω)"
...
  "P" -> "(((A → SKIP) ⟦{}⟧ (B → SKIP)) ⟦{}⟧ SKIP)" [label="C"]
}
```

To type-check, use `csp type`:

```console
$ csp type ./examples/ParABC.sexp "(unwind P)"
P  = ((ParABC) ; ((D::(event )) -> SKIP))

ParABC  = ((((A::(event )) -> SKIP) ||| ((B::(event )) -> SKIP)) ||| ((C::(event )) -> SKIP))
(P)
```
