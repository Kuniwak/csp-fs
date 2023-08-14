module CSP.Core.Sexp.ProcParser

open CSP.Core.Proc
open CSP.Core.ProcShorthand
open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.Mapper
open CSP.Core.Sexp.ProcSyntaxError
open CSP.Core.Util

let atLine line err = At(err, line)
let tryAtom = tryAtom >> Result.mapError MapperError
let trySexps = trySexps >> Result.mapError MapperError
let tryUnaryOrMore = tryUnaryOrMore >> Result.mapError MapperError
let tryBinary = tryBinary >> Result.mapError MapperError
let tryBinaryOrMore = tryBinaryOrMore >> Result.mapError MapperError
let tryTernary = tryTernary >> Result.mapError MapperError
let try4Ary = try4Ary >> Result.mapError MapperError
let parseExpr = ExprParser.parse >> Result.mapError ExprSyntaxError

let rec parseCtor ss =
    tryBinaryOrMore ss
    |> Result.bind (fun (s1, s2, ss) ->
        tryAtom s1
        |> Result.bind (fun ctor ->
            let ss = s2 :: ss in
            let sLast = List.last ss in
            let ss = List.take (List.length ss - 1) ss in

            parse sLast
            |> Result.bind (fun p -> ss |> ResultEx.bindAll tryAtom |> Result.map (fun vars -> ((ctor, vars), p)))))

and parse (sexp: Sexp) : Result<Proc<unit>, ProcSyntaxError> =
    match sexp with
    | Atom("stop", line) -> Ok(stop line)
    | Atom("skip", line) -> Ok(skip line)
    | Atom(str, line) -> Error(UnexpectedKeyword(str)) |> Result.mapError (atLine line)
    | Sexps([], line) -> Error(MapperError(MapperError.UnexpectedEmpty)) |> Result.mapError (atLine line)
    | Sexps(Atom("unwind", line) :: ss, _) ->
        tryUnaryOrMore ss
        |> Result.bind (ResultEx.bind2 tryAtom Ok)
        |> Result.bind (fun (pn, ss) ->
            ss
            |> ResultEx.bindAll parseExpr
            |> Result.map (fun exprs -> unwind pn exprs line))
        |> Result.mapError (atLine line)
    | Sexps(Atom("prefix", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parseExpr parse)
        |> Result.map (fun (expr, p) -> prefix expr p line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("prefixRecv", line) :: ss, _) ->
        tryTernary ss
        |> Result.bind (ResultEx.bind3 parseExpr tryAtom parse)
        |> Result.map (fun (expr, var, p) -> prefixRecv expr var p line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("in", line) :: ss, _) ->
        tryBinaryOrMore ss
        |> Result.bind (ResultEx.bind3 parse parse (ResultEx.bindAll parse))
        |> Result.map (fun (p1, p2, ps) -> List.fold (fun pAcc p -> intCh pAcc p line) (intCh p1 p2 line) ps)
        |> Result.mapError (atLine line)
    | Sexps(Atom("ex", line) :: ss, _) ->
        tryBinaryOrMore ss
        |> Result.bind (ResultEx.bind3 parse parse (ResultEx.bindAll parse))
        |> Result.map (fun (p1, p2, ps) -> List.fold (fun pAcc p -> extCh pAcc p line) (extCh p1 p2 line) ps)
        |> Result.mapError (atLine line)
    | Sexps(Atom("seq", line) :: ss, _) ->
        tryBinaryOrMore ss
        |> Result.bind (ResultEx.bind3 parse parse (ResultEx.bindAll parse))
        |> Result.map (fun (p1, p2, ps) -> List.fold (fun pAcc p -> seq pAcc p line) (seq p1 p2 line) ps)
        |> Result.mapError (atLine line)
    | Sexps(Atom("if", line) :: ss, _) ->
        tryTernary ss
        |> Result.bind (ResultEx.bind3 parseExpr parse parse)
        |> Result.map (fun (expr, p1, p2) -> ``if`` expr p1 p2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("match", line) :: ss, _) ->
        tryUnaryOrMore ss
        |> Result.bind (fun (s, ss) ->
            parseExpr s
            |> Result.bind (fun exprUnion ->
                ss
                |> ResultEx.bindAll trySexps
                |> Result.bind (fun sss -> sss |> ResultEx.bindAll parseCtor)
                |> Result.map (fun xs -> ``match`` exprUnion xs line)))
        |> Result.mapError (atLine line)
    | Sexps(Atom("para", line) :: ss, _) ->
        tryTernary ss
        |> Result.bind (ResultEx.bind3 parse parseExpr parse)
        |> Result.map (fun (p1, expr, p2) -> interfaceParallel p1 expr p2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("interleave", line) :: ss, _) ->
        tryBinaryOrMore ss
        |> Result.bind (ResultEx.bind3 parse parse (ResultEx.bindAll parse))
        |> Result.map (fun (p1, p2, ps) -> List.fold (fun pAcc p -> interleave pAcc p line) (interleave p1 p2 line) ps)
        |> Result.mapError (atLine line)
    | Sexps(Atom("hide", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parse parseExpr)
        |> Result.map (fun (p, expr) -> hide p expr line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("guard", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parseExpr parse)
        |> Result.map (fun (expr, p) -> guard expr p line)
        |> Result.mapError (atLine line)
    | Sexps(Atom(str, line) :: _, _) -> Error(UnexpectedKeyword(str)) |> Result.mapError (atLine line)
    | Sexps(Sexps(ss, line) :: _, _) ->
        Error(MapperError(MapperError.UnexpectedList(ss)))
        |> Result.mapError (atLine line)
