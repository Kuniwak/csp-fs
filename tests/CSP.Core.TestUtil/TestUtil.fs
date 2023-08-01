module CSP.Core.TestUtil

open FSharpPlus

let expand (n: int) (xs: 'a list) : 'a option list =
    List.map Some xs @ List.replicate (max 0 (n - (List.length xs))) None

let zipLongest (xs: 'a list) (ys: 'b list) : ('a option * 'b option) list =
    let n = max (List.length xs) (List.length ys) in
    let xs = expand n xs in
    let ys = expand n ys in
    List.zip xs ys

let cmp (fmt: 'a -> string) (xs: 'a list) (ys: 'a list) : string =
    String.concat
        "\n"
        ("Expected\tActual"
         :: (List.map
             (fun p ->
                 match p with
                 | Some x, Some y -> $"{fmt x}\t{fmt y}"
                 | Some x, None -> $"{fmt x}\t-"
                 | None, Some y -> $"-\t{fmt y}"
                 | None, None -> "-\t-")
             (zipLongest xs ys)))
