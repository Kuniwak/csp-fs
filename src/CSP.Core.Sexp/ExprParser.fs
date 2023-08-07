module CSP.Core.Sexp.ExprParser

open System
open FSharpPlus
open CSP.Core.Util
open CSP.Core.Expr
open CSP.Core.ExprShorthand
open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.Mapper
open CSP.Core.Sexp.ExprSyntaxError


let atLine line err = At(err, line)
let tryAtom = tryAtom >> Result.mapError MapperError
let trySexps = trySexps >> Result.mapError MapperError
let tryUnary = tryUnary >> Result.mapError MapperError
let tryUnaryOrMore = tryUnaryOrMore >> Result.mapError MapperError
let tryBinary = tryBinary >> Result.mapError MapperError
let tryBinaryOrMore = tryBinaryOrMore >> Result.mapError MapperError
let tryTernary = tryTernary >> Result.mapError MapperError
let parseType = TypeParser.parse >> Result.mapError TypeSyntaxError

let parseCtorOrVarRef line str =
    if Char.IsAsciiLetterUpper(Seq.head str) then
        Ok(ctor str [] line)
    else
        Ok(varRef str line)

let rec parseTypeWith2Exprs ss =
    tryTernary ss
    |> Result.bind (fun (s1, s2, s3) ->
        parseType s1
        |> Result.bind (fun t ->
            (s2, s3)
            |> ResultEx.bind2 parse parse
            |> Result.map (fun (e1, e2) -> (t, e1, e2))))

and parseCtor ss =
    tryBinaryOrMore ss
    |> Result.bind (fun (s1, s2, ss) ->
        tryAtom s1
        |> Result.bind (fun ctor ->
            let ss = s2 :: ss in
            let sLast = List.last ss in
            let ss = List.take (List.length ss - 1) ss in

            parse sLast
            |> Result.bind (fun expr ->
                ss |> ResultEx.bindAll tryAtom |> Result.map (fun vars -> ((ctor, vars), expr)))))

and parse (sexp: Sexp) : Result<Expr<unit>, ExprSyntaxError> =
    match sexp with
    | Atom("true", line) -> Ok(litTrue line)
    | Atom("false", line) -> Ok(litFalse line)
    | Atom(str, line) ->
        NatEx.tryParse str
        |> Option.map (fun n -> Ok(litNat n line))
        |> Option.defaultValue (parseCtorOrVarRef line str)
    | Sexps([], line) -> Ok(litUnit line)
    | Sexps(Atom("empty", line) :: ss, _) ->
        tryUnary ss
        |> Result.bind (fun s ->
            TypeParser.parse s
            |> Result.mapError TypeSyntaxError
            |> Result.map (fun t -> litEmpty t line))
        |> Result.mapError (atLine line)
    | Sexps(Atom("tuple", line) :: ss, _) ->
        ss
        |> ResultEx.bindAll parse
        |> Result.bind (fun ts ->
            match List.rev ts with
            | t1 :: t2 :: ts -> Ok(List.fold (fun tAcc t -> tuple2 t tAcc line) (tuple2 t2 t1 line) ts)
            | _ -> Error(TooFewTypeArguments(ss)))
        |> Result.mapError (atLine line)
    | Sexps(Atom("if", line) :: ss, _) ->
        tryTernary ss
        |> Result.bind (ResultEx.bind3 parse parse parse)
        |> Result.map (fun (e1, e2, e3) -> ifExpr e1 e2 e3 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("match", line) :: ss, _) ->
        tryUnaryOrMore ss
        |> Result.bind (fun (s, ss) ->
            parse s
            |> Result.bind (fun exprUnion ->
                ss
                |> ResultEx.bindAll trySexps
                |> Result.bind (fun sss -> sss |> ResultEx.bindAll parseCtor)
                |> Result.map (fun xs -> matchExpr exprUnion xs line)))
        |> Result.mapError (atLine line)
    | Sexps(Atom("eq", line) :: ss, _) ->
        parseTypeWith2Exprs ss
        |> Result.map (fun (t, e1, e2) -> eq t e1 e2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("less", line) :: ss, _) ->
        parseTypeWith2Exprs ss
        |> Result.map (fun (t, e1, e2) -> less t e1 e2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("plus", line) :: ss, _) ->
        parseTypeWith2Exprs ss
        |> Result.map (fun (t, e1, e2) -> plus t e1 e2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("minus", line) :: ss, _) ->
        parseTypeWith2Exprs ss
        |> Result.map (fun (t, e1, e2) -> minus t e1 e2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("times", line) :: ss, _) ->
        parseTypeWith2Exprs ss
        |> Result.map (fun (t, e1, e2) -> times t e1 e2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("size", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parseType parse)
        |> Result.map (fun (t, expr) -> size t expr line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("filter", line) :: ss, _) ->
        try4Ary ss
        |> Result.mapError MapperError
        |> Result.bind (ResultEx.bind4 parseType tryAtom parse parse)
        |> Result.map (fun (t, var, e1, e2) -> filter t var e1 e2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("exists", line) :: ss, _) ->
        try4Ary ss
        |> Result.mapError MapperError
        |> Result.bind (ResultEx.bind4 parseType tryAtom parse parse)
        |> Result.map (fun (t, var, e1, e2) -> exists t var e1 e2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("contains", line) :: ss, _) ->
        tryTernary ss
        |> Result.bind (ResultEx.bind3 parseType parse parse)
        |> Result.map (fun (t, e1, e2) -> contains t e1 e2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("not", line) :: ss, _) ->
        tryUnary ss
        |> Result.bind parse
        |> Result.map (fun expr -> boolNot expr line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("fst", line) :: ss, _) ->
        tryUnary ss
        |> Result.bind parse
        |> Result.map (fun expr -> tupleFst expr line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("snd", line) :: ss, _) ->
        tryUnary ss
        |> Result.bind parse
        |> Result.map (fun expr -> tupleSnd expr line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("cons", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parse parse)
        |> Result.map (fun (expr1, expr2) -> listCons expr1 expr2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("nth", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parse parse)
        |> Result.map (fun (expr1, expr2) -> listNth expr1 expr2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("range", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parse parse)
        |> Result.map (fun (expr1, expr2) -> setRange expr1 expr2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("insert", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parse parse)
        |> Result.map (fun (expr1, expr2) -> setInsert expr1 expr2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("remove", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parse parse)
        |> Result.map (fun (expr1, expr2) -> setRemove expr1 expr2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("add", line) :: ss, _) ->
        tryTernary ss
        |> Result.bind (ResultEx.bind3 parse parse parse)
        |> Result.map (fun (expr1, expr2, expr3) -> mapAdd expr1 expr2 expr3 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("findOpt", line) :: ss, _) ->
        tryBinary ss
        |> Result.bind (ResultEx.bind2 parse parse)
        |> Result.map (fun (expr1, expr2) -> mapFindOpt expr1 expr2 line)
        |> Result.mapError (atLine line)
    | Sexps(Atom("univ", line) :: ss, _) ->
        tryUnary ss
        |> Result.bind (TypeParser.parse >> Result.mapError TypeSyntaxError)
        |> Result.map (fun t -> univ t line)
        |> Result.mapError (atLine line)
    | Sexps(Atom(str, line) :: ss, _) ->
        if List.length ss = 0 then
            parseCtorOrVarRef line str
        else
            ss
            |> ResultEx.bindAll parse
            |> Result.map (fun exprs -> ctor str exprs line)
            |> Result.mapError (atLine line)
    | Sexps(Sexps(ss, line) :: _, _) ->
        Error(MapperError(MapperError.UnexpectedList(ss)))
        |> Result.mapError (atLine line)
