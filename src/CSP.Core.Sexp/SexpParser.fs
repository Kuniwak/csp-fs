module CSP.Core.Sexp.SexpParser

open System
open CSP.Core
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

let skipWhileWithState
    (line: uint)
    (cond: 'State -> char -> bool * 'State)
    (s: 'State)
    (str: char list)
    : char list * uint =
    let rec skipWhile rest line s =
        match rest with
        | [] -> ([], line)
        | c :: rest' ->
            let skip, s = cond s c

            if skip then
                skipWhile rest' (if c = '\n' then line + 1u else line) s
            else
                (rest, line)

    skipWhile str line s

let skipTrivia (line: uint) (str: char list) : char list * uint =
    skipWhileWithState
        line
        (fun inComment c ->
            if inComment then
                (true, c <> '\n')
            else
                let isCommentStart = c = ';' in (isWS c || isCommentStart, isCommentStart))
        false
        str

let skipWhile (line: uint) (cond: char -> bool) (str: char list) : char list * uint =
    skipWhileWithState line (fun _ c -> (cond c, ())) () str

let parseAtom (line: uint) (str: char list) : Result<Sexp * char list * uint, SyntaxError> =
    let cs, rest, line = advanceWhile line (fun c -> c <> ')' && not (isWS c)) str

    if List.isEmpty cs then
        Error(EmptyAtom)
    else
        let rest', line' = skipTrivia line rest in
        Ok(Atom(cs |> Array.ofList |> String, LineNum.ofNat line), rest', line')


let rec parseList (line: uint) (str: char list) : Result<Sexp * char list * uint, SyntaxError> =
    let rec loop xs rest line =
        match parseListOrAtom line rest with
        | Error _ ->
            match rest with
            | [] -> Error(ParensNotClosed)
            | ')' :: rest ->
                let rest', line' = skipTrivia line rest in Ok(Sexps(List.rev xs, LineNum.ofNat line), rest', line')
            | _ -> failwith $"unexpected token: %s{toString rest}"
        | Ok(atom, rest, line) -> loop (atom :: xs) rest line

    match str with
    | '(' :: rest -> let rest, line = skipTrivia line rest in loop [] rest line
    | _ -> failwith $"unexpected token: %s{toString str}"

and parseListOrAtom (line: uint) (str: char list) : Result<Sexp * char list * uint, SyntaxError> =
    match str with
    | [] -> Error(EmptyAtom)
    | '(' :: _ -> parseList line str
    | _ -> parseAtom line str

let parse (str: string) : Result<Sexp, SyntaxError> =
    let rest, line = skipTrivia 1u (str |> toChars) in

    parseListOrAtom line rest
    |> Result.bind (fun (sexp, rest, line) ->
        let rest, _ = skipTrivia line rest in if List.isEmpty rest then Ok(sexp) else Error(GarbageInTail(rest |> toString)))

let parseAll (str: string) : Result<Sexp list, SyntaxError> =
    let rec parseAll xs rest line =
        match parseListOrAtom line rest with
        | Error _ ->
            let rest, _ = skipTrivia line rest in

            if List.isEmpty rest then
                Ok(List.rev xs)
            else
                Error(GarbageInTail(rest |> toString))
        | Ok(atom, rest, line) -> parseAll (atom :: xs) rest line

    let rest, line = skipTrivia 1u (str |> toChars) in
    parseAll [] rest line
