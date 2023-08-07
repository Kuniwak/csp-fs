module CSP.Core.Sexp.TypeParser

open FSharpPlus
open CSP.Core.Util
open CSP.Core.Type
open CSP.Core.TypeShorthand
open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.Mapper
open CSP.Core.Sexp.TypeSyntaxError

let parseTVar (str: string) : Result<TVarId, TypeSyntaxError> =
    if String.startsWith "'" str then
        NatEx.tryParse (String.drop 1 str)
        |> Option.map Ok
        |> Option.defaultValue (Error(InvalidTVarId(str)))
    else
        Error(InvalidTVarId(str))

let tryUnary = tryUnary >> Result.mapError MapperError
let tryBinary = tryBinary >> Result.mapError MapperError
let tryBinaryOrMore = tryBinaryOrMore >> Result.mapError MapperError

let parse (sexp: Sexp) : Result<Type, TypeSyntaxError> =
    let rec parse sexp =
        match sexp with
        | Atom("unit", _) -> Ok(tUnit)
        | Atom("nat", _) -> Ok(tNat)
        | Atom("bool", _) -> Ok(tBool)
        | Atom(str, _) ->
            if String.startsWith "'" str then
                parseTVar str |> Result.map tVar
            else
                Ok(tUnion str [])
        | Sexps(Atom("tuple", _) :: ss, _) ->
            tryBinaryOrMore (List.rev ss)
            |> Result.bind (fun (s1, s2, ss) ->
                (s1, s2)
                |> ResultEx.bind2 parse parse
                |> Result.bind (fun (t1, t2) ->
                    ss
                    |> ResultEx.bindAll parse
                    |> Result.map (List.fold (fun tAcc t -> tTuple2 t tAcc) (tTuple2 t2 t1))))
        | Sexps(Atom("set", _) :: ss, _) -> tryUnary ss |> Result.bind parse |> Result.map tSet
        | Sexps(Atom("list", _) :: ss, _) -> tryUnary ss |> Result.bind parse |> Result.map tList
        | Sexps(Atom("map", _) :: ss, _) -> tryBinary ss |> Result.bind (ResultEx.bind2 parse parse) |> Result.map TMap
        | Sexps(Atom(str, _) :: ss, _) -> ss |> ResultEx.bindAll parse |> Result.map (tUnion str)
        | Sexps([], _) -> Error(MapperError(MapperError.UnexpectedEmpty))
        | Sexps(Sexps(ss, _) :: _, _) -> Error(MapperError(MapperError.UnexpectedList(ss)))

    parse sexp
