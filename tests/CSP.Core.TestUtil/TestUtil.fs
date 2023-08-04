module CSP.Core.TestUtil

open FSharpPlus

let expand (n: int) (xs: 'a seq) : 'a option seq =
    Seq.append (Seq.map Some xs) (Seq.replicate (max 0 (n - (Seq.length xs))) None)

let zipLongest (xs: 'a seq) (ys: 'b seq) : ('a option * 'b option) seq =
    let n = max (Seq.length xs) (Seq.length ys) in
    let xs = expand n xs in
    let ys = expand n ys in
    Seq.zip xs ys

let cmp (fmt: 'a -> string) (xs: 'a list) (ys: 'a list) : string =
    let s =
        zipLongest xs ys
        |> Seq.map (fun p ->
            match p with
            | Some x, Some y -> $"{fmt x}\t{fmt y}"
            | Some x, None -> $"{fmt x}\t-"
            | None, Some y -> $"-\t{fmt y}"
            | None, None -> "-\t-")
        |> String.concat "\n"

    $"""
Expected	Actual
--------	------
%s{s}
"""
