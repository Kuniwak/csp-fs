module CSP.Core.Sexp.SexpParser

open System
open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.SyntaxError

let toString (cs: char list) : string = cs |> Array.ofList |> String
let toChars (s: string) = Seq.toList s

let isWS (c: char) =
    match c with
    | ' ' -> true
    | '\t' -> true
    | '\n' -> true
    | '\r' -> true
    | _ -> false

let advance (str: char list) : (char * char list) option =
    match str with
    | [] -> None
    | c :: cs -> Some(c, cs)

let advanceWhile (line: uint) (cond: char -> bool) (str: char list) : char list * char list * uint =
    let rec advanceWhile cs rest line =
        match rest with
        | [] -> (cs, [], line)
        | c :: cs' ->
            if cond c then
                advanceWhile (c :: cs) cs' (if c = '\n' then line + 1u else line)
            else
                (cs, rest, line)

    let cs, rest, line = advanceWhile [] str line in
    (List.rev cs, rest, line)

let skipWhile (line: uint) (cond: char -> bool) (str: char list) : char list * uint =
    let rec skipWhile rest line =
        match rest with
        | [] -> ([], line)
        | c :: rest' ->
            if cond c then
                skipWhile rest' (if c = '\n' then line + 1u else line)
            else
                (rest, line)

    skipWhile str line

let parseAtom (line: uint) (str: char list) : Result<Sexp * char list * uint, SyntaxError> =
    let cs, rest, line = advanceWhile line (fun c -> c <> ')' && not (isWS c)) str

    if List.isEmpty cs then
        Error(EmptyAtom)
    else
        let rest, line = skipWhile line isWS rest in Ok(Atom(cs |> Array.ofList |> String, $"%d{line}"), rest, line)

let rec parseList (line: uint) (str: char list) : Result<Sexp * char list * uint, SyntaxError> =
    let rec loop xs rest line =
        match parse line rest with
        | Error _ ->
            match rest with
            | [] -> Error(ParensNotClosed)
            | ')' :: rest -> Ok(List(List.rev xs, $"%d{line}"), rest, line)
            | _ -> failwith $"unexpected token: %s{toString rest}"
        | Ok(atom, rest, line) -> loop (atom :: xs) rest line

    match str with
    | '(' :: rest -> loop [] rest line
    | _ -> failwith $"unexpected token: %s{toString str}"

and parse (line: uint) (str: char list) : Result<Sexp * char list * uint, SyntaxError> =
    match str with
    | [] -> Error(EmptyAtom)
    | '(' :: _ -> let rest, line = skipWhile line isWS str in parseList line rest
    | _ -> parseAtom line str
