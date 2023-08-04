module CSP.Core.Util.MapEx

let tryFold1 (folder: 'State -> 'K -> 'V -> 'State) (init: 'K -> 'V -> 'State) (m: Map<'K, 'V>) : 'State option =
    let kvs = Map.toList m in

    match List.tryHead kvs with
    | None -> None
    | Some(k, v) -> let kvs = List.tail kvs in Some(List.fold (fun acc (k, v) -> folder acc k v) (init k v) kvs)

let fold1 (folder: 'State -> 'K -> 'V -> 'State) (init: 'K -> 'V -> 'State) = Option.get >> (tryFold1 folder init)

let tryFrom (xs: ('k * 'v) seq) : Result<Map<'k, 'v>, Set<'k>> =
    Seq.fold
        (fun mRes (k, v) ->
            match mRes with
            | Ok(m) ->
                if Map.containsKey k m then
                    Error(Set.singleton k, Set.ofSeq (Map.keys m))
                else
                    Ok(Map.add k v m)
            | Error(dup, s) ->
                if Set.contains k s then
                    Error(Set.add k dup, s)
                else
                    Error(dup, Set.add k s))
        (Ok(Map.empty))
        xs
    |> Result.mapError snd
