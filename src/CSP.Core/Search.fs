module CSP.Core.Search

type SearchConfig = { NodeMax: int }

let searchConfig n = { NodeMax = n }
type typeToGetMaxValue = int32
let searchCfgUnlimited = { NodeMax = typeToGetMaxValue.MaxValue }

let search
    (cfg: SearchConfig)
    (visit: 's -> ('e * 's) list -> Unit)
    (next: 's -> ('e * 's) list)
    (joinQueue: ('e * 's) list -> 's list -> 's list)
    (norm: 's -> 's)
    (ns: 's list)
    (visited: Set<'s>)
    : Unit =
    let rec search ns visited =
        match ns with
        | [] -> ()
        | n :: ns' ->
            let n = norm n in

            if Set.count visited < cfg.NodeMax && not (Set.contains n visited) then
                let es = next n in
                visit n es
                search (joinQueue es ns') (Set.add n visited)
            else
                search ns' visited in

    search ns visited

let dfs cfg visit next norm n =
    search cfg visit next (fun ts rest -> (List.map snd ts) @ rest) norm [ n ] Set.empty

let bfs cfg visit next norm n =
    search cfg visit next (fun ts rest -> rest @ (List.map snd ts)) norm [ n ] Set.empty
