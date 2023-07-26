module CSP.Core.Util.MapEx

let tryFold1 (folder: 'State -> 'K -> 'V -> 'State) (init: 'K -> 'V -> 'State) (m: Map<'K, 'V>) : 'State option =
    let kvs = Map.toList m in

    match List.tryHead kvs with
    | None -> None
    | Some(k, v) -> let kvs = List.tail kvs in Some(List.fold (fun acc (k, v) -> folder acc k v) (init k v) kvs)

let fold1 (folder: 'State -> 'K -> 'V -> 'State) (init: 'K -> 'V -> 'State) = Option.get >> (tryFold1 folder init)
