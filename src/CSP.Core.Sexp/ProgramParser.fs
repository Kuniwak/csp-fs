module CSP.Core.Sexp.ProgramParser

open CSP.Core
open CSP.Core.CtorMap
open CSP.Core.Expr
open CSP.Core.GlobalEnv
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.UnionMap
open CSP.Core.Sexp.Stmt
open CSP.Core.Sexp.SexpParser
open CSP.Core.Sexp.ProgramSyntaxError
open CSP.Core.Util

let parseExpr = ExprParser.parse >> Result.mapError ExprSyntaxError
let parseStmt = StmtParser.parse >> Result.mapError StmtSyntaxError

let parse (s: string) : Result<ProcMap<unit> * UnionMap * CtorMap * GlobalEnv, ProgramSyntaxError> =
    parseAll s
    |> Result.mapError SyntaxError
    |> Result.bind (ResultEx.bindAll parseStmt)
    |> Result.bind (fun stmts ->
        List.foldBack
            (fun stmt (pm, um, genv) ->
                match stmt with
                | ProcDecl x -> (x :: pm, um, genv)
                | UnionDecl x -> (pm, x :: um, genv)
                | GlobalVarDecl x -> (pm, um, x :: genv))
            stmts
            ([], [], [])
        |> ResultEx.bind3 (ProcMap.from >> Result.mapError ProcMapError) (from >> Result.mapError UnionMapError) Ok)
    |> Result.bind (fun (pm, um, genv) ->
        CtorMap.from um
        |> Result.mapError CtorMapError
        |> Result.bind (fun cm ->
            GlobalEnv.from genv
            |> Result.mapError GlobalEnvError
            |> Result.map (fun genv -> (pm, um, cm, genv))))

let parseInit (p: string) : Result<Proc<unit>, ProgramSyntaxError> =
    SexpParser.parse p
    |> Result.mapError SyntaxError
    |> Result.bind (ProcParser.parse >> Result.mapError ProcSyntaxError)
