module CSP.Core.Util.ResultEx

let get (fmt: 'E -> string) (res: Result<'V, 'E>) : 'V =
    match res with
    | Ok(v) -> v
    | Error(err) -> failwith (fmt err)
