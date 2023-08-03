module CSP.Core.Util.ResultEx

let get (fmt: 'E -> string) (res: Result<'V, 'E>) : 'V =
    match res with
    | Ok(v) -> v
    | Error(err) -> failwith (fmt err)

let bind2 (f1: 'a -> Result<'b, 'e>) (f2: 'c -> Result<'d, 'e>) (x: 'a * 'c) : Result<'b * 'd, 'e> =
    f1 (fst x) |> Result.bind (fun c -> f2 (snd x) |> Result.map (fun d -> (c, d)))

let bind3
    (f1: 'a -> Result<'b, 'e>)
    (f2: 'c -> Result<'d, 'e>)
    (f3: 'f -> Result<'g, 'e>)
    (x: 'a * 'c * 'f)
    : Result<'b * 'd * 'g, 'e> =
    let fst, snd, trd = x in

    (fst, snd)
    |> bind2 f1 f2
    |> Result.bind (fun (a, c) -> f3 trd |> Result.map (fun g -> (a, c, g)))

let bindAll (f: 'a -> Result<'c, 'b>) (xs: 'a seq) : Result<'c list, 'b> =
    Seq.foldBack (fun x -> Result.bind (fun ys -> Result.map (fun y -> y :: ys) (f x))) xs (Ok([]))

let mapAll (f: 'a -> 'c) (xs: 'a seq) : Result<'c list, 'b> =
    Seq.foldBack (fun x -> Result.map (fun ys -> (f x) :: ys)) xs (Ok([]))
