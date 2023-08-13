module CSP.Core.Sexp.ProgramSyntaxError

open CSP.Core
open CSP.Core.CtorMapError
open CSP.Core.GlobalEnvError
open CSP.Core.UnionMapError
open CSP.Core.ProcMapError
open CSP.Core.Sexp.Stmt
open CSP.Core.Sexp.SyntaxError
open CSP.Core.Sexp.ProcSyntaxError
open CSP.Core.Sexp.ExprSyntaxError
open CSP.Core.Sexp.StmtSyntaxError

type ProgramSyntaxError =
    | SyntaxError of SyntaxError
    | StmtSyntaxError of StmtSyntaxError
    | ExprSyntaxError of ExprSyntaxError
    | ProcSyntaxError of ProcSyntaxError
    | CtorMapError of CtorMapError
    | GlobalEnvError of GlobalEnvError
    | NoInit
    | TooMuchInits of Stmt list
    | ProcMapError of ProcMapError
    | UnionMapError of UnionMapError

let format (err: ProgramSyntaxError) : string =
    match err with
    | SyntaxError(err) -> SyntaxError.format err
    | StmtSyntaxError(err) -> format err
    | ExprSyntaxError(err) -> ExprSyntaxError.format err
    | ProcSyntaxError(err) -> ProcSyntaxError.format err
    | CtorMapError(err) -> CtorMapError.format err
    | GlobalEnvError(err) -> GlobalEnvError.format err
    | ProcMapError(err) -> ProcMapError.format err
    | UnionMapError(err) -> UnionMapError.format err
    | NoInit -> "no init exist"
    | TooMuchInits(ss) ->
        let s = ss |> Seq.map (fun s -> $"\t%s{Stmt.format s}") |> String.concat "\n" in

        $"too much init:\n%s{s}"
