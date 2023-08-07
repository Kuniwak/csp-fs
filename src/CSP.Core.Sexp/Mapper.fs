module CSP.Core.Sexp.Mapper

open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.MapperError

let tryLength (ss: Sexp list) (len: int) : Result<Sexp list, MapperError> =
    if List.length ss = len then
        Ok(ss)
    else if List.length ss > len then
        Error(TooMuchArguments(ss))
    else
        Error(TooFewArguments(ss))

let tryUnary (ss: Sexp list) : Result<Sexp, MapperError> =
    tryLength ss 1
    |> Result.map (fun ss ->
        match ss with
        | [ s ] -> s
        | _ -> failwith $"unexpected length: %d{List.length ss}")

let tryUnaryOrMore (ss: Sexp list) : Result<Sexp * Sexp list, MapperError> =
    if List.length ss < 1 then
        Error(TooFewArguments(ss))
    else
        match ss with
        | s::ss -> Ok(s, ss)
        | _ -> failwith $"unexpected length: %d{List.length ss}"

let tryBinary (ss: Sexp list) : Result<Sexp * Sexp, MapperError> =
    tryLength ss 2
    |> Result.map (fun ss ->
        match ss with
        | [ s1; s2 ] -> (s1, s2)
        | _ -> failwith $"unexpected length: %d{List.length ss}")

let tryBinaryOrMore (ss: Sexp list) : Result<Sexp * Sexp * Sexp list, MapperError> =
    if List.length ss < 2 then
        Error(TooFewArguments(ss))
    else
        match ss with
        | s1::s2::ss -> Ok(s1, s2, ss)
        | _ -> failwith $"unexpected length: %d{List.length ss}"

let tryTernary (ss: Sexp list) : Result<Sexp * Sexp * Sexp, MapperError> =
    tryLength ss 3
    |> Result.map (fun ss ->
        match ss with
        | [ s1; s2; s3 ] -> (s1, s2, s3)
        | _ -> failwith $"unexpected length: %d{List.length ss}")

let try4Ary (ss: Sexp list) : Result<Sexp * Sexp * Sexp * Sexp, MapperError> =
    tryLength ss 4
    |> Result.map (fun ss ->
        match ss with
        | [ s1; s2; s3; s4 ] -> (s1, s2, s3, s4)
        | _ -> failwith $"unexpected length: %d{List.length ss}")

let tryAtom sexp =
    match sexp with
    | Atom(str, _) -> Ok(str)
    | Sexps(ss, _) -> Error(UnexpectedList(ss))

let trySexps sexp =
    match sexp with
    | Sexps(ss, _) -> Ok(ss)
    | Atom(s, _) -> Error(UnexpectedAtom(s))
