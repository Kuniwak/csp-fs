module CSP.Core.Sexp.Stmt

open CSP.Core
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.Type

type Stmt =
    | ProcDecl of ((string * (string * Type) list) * Proc<unit>)
    | UnionDecl of ((TVarId list * UnionName) * (string * Type list) list)
    | Init of (string * Expr<unit> list)

let format (stmt: Stmt) : string =
    match stmt with
    | ProcDecl((pn, varDecls), p) ->
        let s =
            varDecls
            |> Seq.map (fun (var, t) -> $"(%s{var} %s{format t})")
            |> String.concat " " in

        $"(def %s{pn} (%s{s}) %s{Proc.format noAnnotation p})"
    | UnionDecl((tVars, un), ctorDecls) ->
        let s1 = tVars |> Seq.map (TVar >> format) |> String.concat " " in

        let s2 =
            ctorDecls
            |> Seq.map (fun (ctor, ts) -> let s = ts |> Seq.map format |> String.concat " " in $"(%s{ctor} %s{s})")
            |> String.concat " " in

        $"(type (%s{s1}) %s{un} %s{s2})"
    | Init(s, exprs) ->
        let s' = exprs |> Seq.map (Expr.format noAnnotation) |> String.concat " "
        $"(init %s{s} %s{s'})"
