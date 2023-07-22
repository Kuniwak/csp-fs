module CSP.Core.Util.ListEx

let allOrNothing (opts: 'a option list) : 'a list option =
    List.foldBack
        (fun opt acc ->
            match acc, opt with
            | Some xs, Some x -> Some(x :: xs)
            | _ -> None)
        opts
        (Some [])

let rec cartesian2 (xs: 'a list) (ys: 'b list) : ('a * 'b) list = List.allPairs xs ys

let rec cartesian (xss: 'a list list) : 'a list list =
    List.foldBack (fun xs yss -> List.map (fun (ys, x) -> x :: ys) (cartesian2 yss xs)) xss [ [] ]


let power (xs: 'a list) : 'a list list =
    List.map
        (fun xOpts ->
            List.collect
                (fun xOpt ->
                    match xOpt with
                    | Some x -> [ x ]
                    | None -> [])
                xOpts)
        (cartesian (List.map (fun v -> [ None; Some v ]) xs))

