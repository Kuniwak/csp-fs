module CSP.Core.Search

let rec search
    (visit: 's -> ('e * 's) list -> Unit)
    (max: int)
    (next: 's -> ('e * 's) list)
    (combine: ('e * 's) list -> 's list -> 's list)
    (norm: 's -> 's)
    (ns: 's list)
    (visited: Set<'s>)
    =
    match ns with
    | [] -> ()
    | n :: ns' ->
        let n = norm n in
        if Set.count visited < max && not (Set.contains n visited) then
            let visited = Set.add n visited
            let es = next n
            visit n es
            search visit max next combine norm (combine es ns') visited
        else
            search visit max next combine norm ns' visited

let dfs visit max next norm n =
    search visit max next (fun ts rest -> (List.map snd ts) @ rest) norm [ n ] Set.empty

let bfs visit max next norm n =
    search visit max next (fun ts rest -> rest @ (List.map snd ts)) norm [ n ] Set.empty