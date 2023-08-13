module CSP.Core.Sexp.StmtParser

open CSP.Core.Expr
open CSP.Core.Sexp.Stmt
open CSP.Core.Type
open CSP.Core.Sexp.StmtSyntaxError
open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.Mapper
open CSP.Core.Util


let atLine line err = At(err, line)
let tryUnaryOrMore = tryUnaryOrMore >> Result.mapError MapperError
let tryBinary = tryBinary >> Result.mapError MapperError
let tryBinaryOrMore = tryBinaryOrMore >> Result.mapError MapperError
let tryTernary = tryTernary >> Result.mapError MapperError
let tryAtom = tryAtom >> Result.mapError MapperError
let trySexps = trySexps >> Result.mapError MapperError
let parseProc = ProcParser.parse >> Result.mapError ProcSyntaxError
let parseExpr = ExprParser.parse >> Result.mapError ExprSyntaxError
let parseType = TypeParser.parse >> Result.mapError TypeSyntaxError

let parseVarDecl (sexp: Sexp) : Result<string * Type, StmtSyntaxError> =
    trySexps sexp
    |> Result.bind tryBinary
    |> Result.bind (ResultEx.bind2 tryAtom parseType)

let parseVarDecls (sexp: Sexp) : Result<(string * Type) list, StmtSyntaxError> =
    trySexps sexp |> Result.bind (ResultEx.bindAll parseVarDecl)

let parseTVar (sexp: Sexp) : Result<TVarId, StmtSyntaxError> =
    tryAtom sexp
    |> Result.bind (TypeParser.parseTVar >> Result.mapError TypeSyntaxError)

let parseTVars (sexp: Sexp) : Result<TVarId list, StmtSyntaxError> =
    trySexps sexp |> Result.bind (ResultEx.bindAll parseTVar)

let parseExprs (s: Sexp) : Result<Expr<unit> list, StmtSyntaxError> =
    trySexps s |> Result.bind (ResultEx.bindAll parseExpr)

let parseCtor (s: Sexp) : Result<string * Type list, StmtSyntaxError> =
    match s with
    | Atom(str, _) -> Ok(str, [])
    | Sexps(ss, _) ->
        tryUnaryOrMore ss
        |> Result.bind (ResultEx.bind2 tryAtom (ResultEx.bindAll parseType))

let parse (sexp: Sexp) : Result<Stmt, StmtSyntaxError> =
    trySexps sexp
    |> Result.bind (fun ss ->
        match ss with
        | [] -> Error(MapperError(MapperError.UnexpectedEmpty))
        | Atom("def", line) :: ss ->
            tryTernary ss
            |> Result.bind (ResultEx.bind3 tryAtom parseVarDecls parseProc)
            |> Result.map (fun (pn, varDecls, p) -> ProcDecl((pn, varDecls), p))
            |> Result.mapError (atLine line)
        | Atom("type", line) :: ss ->
            tryBinaryOrMore ss
            |> Result.bind (ResultEx.bind3 parseTVars tryAtom (ResultEx.bindAll parseCtor))
            |> Result.map (fun (tVars, un, ctorDecls) -> UnionDecl((tVars, un), ctorDecls))
            |> Result.mapError (atLine line)
        | Atom("global", line) :: ss ->
            tryBinary ss
            |> Result.bind (ResultEx.bind2 tryAtom parseExpr)
            |> Result.map GlobalVarDecl
            |> Result.mapError (atLine line)
        | Atom(str, line) :: _ -> Error(UnexpectedKeyword(str)) |> Result.mapError (atLine line)
        | Sexps(ss, line) :: _ ->
            Error(MapperError(MapperError.UnexpectedList(ss)))
            |> Result.mapError (atLine line))
