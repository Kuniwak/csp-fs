module CSP.Core.Util.ResultEx

let get (fmt: 'E -> string) (res: Result<'V, 'E>) : 'V =
    match res with
    | Ok(v) -> v
    | Error(err) -> failwith (fmt err)

let bindAll (f: 'a -> Result<'c, 'b>) (xs: 'a seq) : Result<'c list, 'b> =
    Seq.foldBack (fun x -> Result.bind (fun ys -> Result.map (fun y -> y :: ys) (f x))) xs (Ok([]))

let mapAll (f: 'a -> 'c) (xs: 'a seq) : Result<'c list, 'b> =
    Seq.foldBack (fun x -> Result.map (fun ys -> (f x) :: ys)) xs (Ok([]))
