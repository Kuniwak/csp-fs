module CSP.Core.Sexp.ProgramSyntaxError

open CSP.Core
open CSP.Core.UnionMapError
open CSP.Core.ProcMapError
open CSP.Core.Sexp.Stmt
open CSP.Core.Sexp.SyntaxError
open CSP.Core.Sexp.StmtSyntaxError

type ProgramSyntaxError =
    | SyntaxError of SyntaxError
    | StmtSyntaxError of StmtSyntaxError
    | NoInit
    | TooMuchInits of Stmt list
    | ProcMapError of ProcMapError
    | UnionMapError of UnionMapError

let format (err: ProgramSyntaxError) : string =
    match err with
    | SyntaxError(err) -> SyntaxError.format err
    | StmtSyntaxError(err) -> format err
    | ProcMapError(err) -> ProcMapError.format err
    | UnionMapError(err) -> UnionMapError.format err
    | NoInit -> "no init exist"
    | TooMuchInits(ss) ->
        let s = ss |> Seq.map (fun s -> $"\t%s{Stmt.format s}") |> String.concat "\n" in

        $"too much init:\n%s{s}"
