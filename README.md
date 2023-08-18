CSP Interpreter
===============

Implementation of [Communicating Sequential Proccesses (CSP)](https://ja.wikipedia.org/wiki/Communicating_Sequential_Processes).



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
  c 0: A -> ((SKIP ⟦{}⟧ (B → SKIP)) ⟦{}⟧ (C → SKIP))
  c 1: B -> (((A → SKIP) ⟦{}⟧ SKIP) ⟦{}⟧ (C → SKIP))
  c 2: C -> (((A → SKIP) ⟦{}⟧ (B → SKIP)) ⟦{}⟧ SKIP)

> c0
state: ((SKIP ⟦{}⟧ (B → SKIP)) ⟦{}⟧ (C → SKIP))
  c 0: τ -> ((Ω ⟦{}⟧ (B → SKIP)) ⟦{}⟧ (C → SKIP))
  c 1: B -> ((SKIP ⟦{}⟧ SKIP) ⟦{}⟧ (C → SKIP))
  c 2: C -> ((SKIP ⟦{}⟧ (B → SKIP)) ⟦{}⟧ SKIP)

> c1
state: ((SKIP ⟦{}⟧ SKIP) ⟦{}⟧ (C → SKIP))
  c 0: τ -> ((Ω ⟦{}⟧ SKIP) ⟦{}⟧ (C → SKIP))
  c 1: τ -> ((SKIP ⟦{}⟧ Ω) ⟦{}⟧ (C → SKIP))
  c 2: C -> ((SKIP ⟦{}⟧ SKIP) ⟦{}⟧ SKIP)

> c2

...

> quit
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
P  = (seq (unwind ParABC) (prefix (D::event) skip))

ParABC  = (interleave (interleave (prefix (A::event) skip) (prefix (B::event) skip)) (prefix (C::event) skip))
(unwind P)
```



Syntax
------

Supported S-expression syntax is:

```peg
Comment ← ';' [^\n]* '\n'
WS      ← [\t\r\n ]
Trivia  ← (WS / Comment)+

Atom ← [^)\t\r\n ]+ Trivia?
List ←'(' Trivia? (List / Atom)* ')' Trivia?

File ← Trivia? (List / Atom)
```



### Top-level

Program must have the following data at the top level:

| Data                                 | Description                                                          |
|:-------------------------------------|:---------------------------------------------------------------------|
| `(proc <procName> (<var>*) <proc>)`  | Process declaration                                                  |
| `(type <typeName> (<ctor> <type>)+)` | [ADT](https://en.wikipedia.org/wiki/Algebraic_data_type) declaration |
| `(const <varName> <expr>)`           | Constant declaration                                                 |

At least one process declaration needed.



### Processes

`<proc>` is one of the following data:

| Data                                                       | Description                                                                                                               |
|:-----------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------|
| `stop`                                                     | _STOP_ process.                                                                                                           |
| `skip`                                                     | _SKIP_ process.                                                                                                           |
| `(unwind <procName> <arg>*)`                               | Unwinding.                                                                                                                |
| `(prefix <expr> <proc>)`                                   | Prefixing.                                                                                                                |
| `(prefix <expr> <var> <proc>)`                             | Prefixing. Receiving one of an event that is in a set that is from `<expr>`. The received event is binded to `<varName>`. |
| `(in <proc> <proc>+)`                                      | Internal choice (also called as non-deterministic choice).                                                                |
| `(ex <proc> <proc>+)`                                      | External choice (also called as deterministic choice).                                                                    |
| `(seq <proc> <proc>+)`                                     | Sequential composition.                                                                                                   |
| `(if <expr> <proc> <proc>)`                                | Boolean conditionial.                                                                                                     |
| `(match <expr> (<ctor> <var>* <proc>)+ (_ <var> <proc>)?)` | Pattern match.                                                                                                            |
| `(para <proc> <expr> <proc>)`                              | Interface parallel.                                                                                                       |
| `(interleave <proc>*)`                                     | Interleaving.                                                                                                             |
| `(hide <proc> <expr>)`                                     | Hiding events from `<expr>`.                                                                                              |
| `(guard <expr> <proc>)`                                    | Guard. Shorthand of `(if <expr> <proc> stop)`.                                                                            |



### Expressions

`<expr>` is one of the following data:

| Syntax                                                      | Description                                                                                                                                                                                                    |
|:------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `true`                                                      | True value literal.                                                                                                                                                                                            |
| `false`                                                     | False value literal.                                                                                                                                                                                           |
| `[0-9]+`                                                    | Natural number literal.                                                                                                                                                                                        |
| `[a-z][^ \t\r\n()]*`                                        | Variable reference.                                                                                                                                                                                            |
| `[A-Z][^ \t\r\n()]*`                                        | Data constructor.                                                                                                                                                                                              |
| `(empty <type>)`                                            | Empty value literal. Available types are: `(list '0)`, `(set '0)`, `(map '0 '1)`.                                                                                                                              |
| `(tuple <expr> <expr>+)`                                    | Tuple expression.                                                                                                                                                                                              |
| `(if <expr> <expr> <expr>)`                                 | Conditional expression.                                                                                                                                                                                        |
| `(match <expr> (<ctor> <var>* <expr>)+ (_ <var> <expr>)?) ` | Patten match expression. Can match only ADTs, not tuples or booleans or natural numbers or lists.                                                                                                              |
| `(eq <type> <expr> <expr>)`                                 | Equality operator.                                                                                                                                                                                             |
| `(less <type> <expr> <expr>)`                               | Strict comparison operator. Available types are: `ord`. `ord` is one of the types; `unit`, `nat`, `(tuple ord ord)`, `(set '0)`.                                                                               |
| `(plus <type> <expr> <expr>)`                               | Additive operator. Available types are `plus`. `plus` is one of the types; `unit`, `(tuple plus plus)`, `bool`, `nat`, `(set '0)`, `(list '0)`.                                                                |
| `(minus <type> <expr> <expr>)`                              | Subtractive operator. Available types are `minus`. `minus` is one of the types; `unit`, `(tuple minus minus)`, `nat`, `(set '0)`.                                                                              |
| `(times <type> <expr> <expr>)`                              | Times operator. Available types are `times`. `times` is one of the types; `bool`, `nat`, `(set '0)`.                                                                                                           |
| `(size <type> <expr>)`                                      | Size operator. Available types are `enum`. `enum` is one of the types; `(set '0)`, `(list '0)`, `(map '0 '1)`.                                                                                                 |
| `(filter <type> <var> <expr> <expr>)`                       | Filter operator. Available types are `enum`. `enum` is one of the types; `(set '0)`, `(list '0)`, `(map '0 '1)`. The variable name and the first expression is a lambda that must return a boolean value.      |
| `(exists <type> <var> <expr> <expr>)`                       | Existential operator. Available types are `enum`. `enum` is one of the types; `(set '0)`, `(list '0)`, `(map '0 '1)`. The variable name and the first expression is a lambda that must return a boolean value. |
| `(contains <type> <expr> <expr>)`                           | Search operator. Available types are `enum`. `enum` is one of the types; `(set '0)`, `(list '0)`, `(map '0 '1)`.                                                                                               |
| `(not <expr>)`                                              | Boolean NOT operator.                                                                                                                                                                                          |
| `(fst <expr>)`                                              | First element of the tuple.                                                                                                                                                                                    |
| `(snd <expr>)`                                              | Second element of the tuple.                                                                                                                                                                                   |
| `(cons <expr> <expr>)`                                      | Cons operator.                                                                                                                                                                                                 |
| `(nth <expr> <expr>)`                                       | Nth operator.                                                                                                                                                                                                  |
| `(range <expr> <expr>)`                                     | Left-closed, right-open interval operator. This operator returns a set of natural numbers.                                                                                                                     |
| `(insert <expr> <expr>)`                                    | Inserts an element to the set.                                                                                                                                                                                 |
| `(remove <expr> <expr>)`                                    | Removes an element of the set. Do nothing if the element not in the set.                                                                                                                                       |
| `(add <expr> <expr> <expr>)`                                | Adds a new key value pair to the map.                                                                                                                                                                          |
| `(findOpt <expr> <expr>)`                                   | Find the specified key from the map. Returns `(Some x)` if the specified key found. Otherwise returns `None`.                                                                                                  |
| `(univ <type>)`                                             | Return a subset of the universal set of the specified type. The set controlled by options `--nat-max` and `--list-max`.                                                                                        |



### Types

`<type>` is one of the following data:

| Data                     | Description                                                              |
|:-------------------------|:-------------------------------------------------------------------------|
| `'[0-9]+`                | Type parameter.                                                          |
| `unit`                   | Unit type.                                                               |
| `nat`                    | Natural number type.                                                     |
| `bool`                   | Boolean type.                                                            |
| `(option <type>)`        | Option type.                                                             |
| `(either <type> <type>)` | Either type.                                                             |
| `(tuple <type> <type>+)` | Tuple type.                                                              |
| `(set <type>)`           | Set type.                                                                |
| `(list <type>)`          | List type.                                                               |
| `(map <type> <type>)`    | Map type. The first type is a key type, the second type is a value type. |



### Data constructors

`<ctor>` is `[A-Z][^ \t\r\n()]*`.



# Acknowledgement

- [並行システムの検証と実装：形式手法CSPに基づく高信頼並行システム開発入門（トップエスイー実践講座6）, 本位田 真一, 東野 輝夫(監修), 磯部 祥尚(著)](https://tatsu-zine.com/books/verification-and-implementation-of-concurrent-systems)
