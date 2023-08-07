module CSP.Core.Sexp.Sexp

open CSP.Core.LineNum
open CSP.Core.Util

type Sexp =
    | Atom of string * LineNum
    | Sexps of Sexp list * LineNum

let line (sexp: Sexp) : LineNum =
    match sexp with
    | Atom(_, line) -> line
    | Sexps(_, line) -> line

let rec format (sexp: Sexp) : string =
    match sexp with
    | Atom(s, _) -> s
    | Sexps(ss, _) -> ss |> Seq.map format |> String.concat " " |> StringEx.wrapBy "(" ")"
