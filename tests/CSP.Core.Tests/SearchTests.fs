module CSP.Core.Tests.SearchTests

open CSP.Core.Search

open Xunit

let mapNext (m: Map<'n, ('e * 'n) list>) (n: 'n) : ('e * 'n) list = Map.find n m

[<Fact>]
let dfs_rec () =
    let next = mapNext (Map [ ("a", [ ((), "a") ]) ]) in
    let mutable visited = [] in
    dfs (fun n _ -> visited <- visited @ [ n ]) 100 next id "a"
    Assert.Equal<string list>([ "a" ], visited)

[<Fact>]
let bfs_rec () =
    let next = mapNext (Map [ ("a", [ ((), "a") ]) ]) in
    let mutable visited = [] in
    bfs (fun n _ -> visited <- visited @ [ n ]) 100 next id "a"
    Assert.Equal<string list>([ "a" ], visited)

[<Fact>]
let dfs_order () =
    let next =
        mapNext (
            Map
                [ ("a", [ ((), "a-1"); ((), "a-2") ])
                  ("a-1", [ ((), "a-1-1") ])
                  ("a-1-1", [])
                  ("a-2", []) ]
        ) in

    let mutable visited = [] in
    dfs (fun n _ -> visited <- visited @ [ n ]) 100 next id "a"
    Assert.Equal<string list>([ "a"; "a-1"; "a-1-1"; "a-2" ], visited)

[<Fact>]
let bfs_order () =
    let next =
        mapNext (
            Map
                [ ("a", [ ((), "a-1"); ((), "a-2") ])
                  ("a-1", [ ((), "a-1-1") ])
                  ("a-1-1", [])
                  ("a-2", []) ]
        ) in

    let mutable visited = [] in
    bfs (fun n _ -> visited <- visited @ [ n ]) 100 next id "a"
    Assert.Equal<string list>([ "a"; "a-1"; "a-2"; "a-1-1" ], visited)
