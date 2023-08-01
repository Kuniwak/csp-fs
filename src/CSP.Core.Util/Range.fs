module CSP.Core.Util.Range

let rec ofSet (n1: uint) (n2: uint) : Set<uint> =
    if n1 > n2 then failwith "n1 > n2"
    else if n1 = n2 then Set.empty
    else Set.add n1 (ofSet (n1 + 1u) n2)

let ofList (n1: uint) (n2: uint) : uint list =
    if n1 > n2 then
        failwith $"the first arg must not be greater than the second one: range {n1} {n2}"

    let rec loop n1 n2 =
        if n1 = n2 then [] else n1 :: (loop (n1 + 1u) n2) in

    loop n1 n2
