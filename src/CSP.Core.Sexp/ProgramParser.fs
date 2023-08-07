module CSP.Core.Sexp.ProgramParser

open CSP.Core
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.ProcMap
open CSP.Core.UnionMap
open CSP.Core.Sexp.SexpParser
open CSP.Core.Sexp.ProgramSyntaxError
open CSP.Core.Sexp.Stmt
open CSP.Core.Util

let parseStmt = StmtParser.parse >> Result.mapError StmtSyntaxError

let parse (s: string) : Result<ProcMap<unit> * UnionMap * (ProcId * Expr<unit> list), ProgramSyntaxError> =
    parseAll s
    |> Result.mapError SyntaxError
    |> Result.bind (ResultEx.bindAll parseStmt)
    |> Result.bind (fun stmts ->
        let pm, um, im =
            List.foldBack
                (fun stmt (pm, um, im) ->
                    match stmt with
                    | ProcDecl x -> (x :: pm, um, im)
                    | UnionDecl x -> (pm, x :: um, im)
                    | Init x -> (pm, um, x :: im))
                stmts
                ([], [], []) in

        if List.length im < 1 then
            Error(NoInit)
        else if List.length im > 1 then
            Error(TooMuchInits(im |> List.map Init))
        else
            (pm, um)
            |> ResultEx.bind2 (ProcMap.from >> Result.mapError ProcMapError) (from >> Result.mapError UnionMapError)
            |> Result.map (fun (pm, um) -> (pm, um, List.head im)))
