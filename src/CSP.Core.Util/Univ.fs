module CSP.Core.Util.Univ

let ofSet (s: Set<'a>) : Set<'a> list =
    List.map Set (ListEx.power (Set.toList s))


let rec ofList (xs: 'a list) (l: uint) : 'a list list =
    if List.isEmpty xs then
        failwith $"must not be empty: %s{nameof ofList}"
    else
        List.foldBack (fun _ xss -> List.map (fun (x, xs) -> x :: xs) (List.allPairs xs xss)) (Range.ofList 0u l) [ [] ]

let rec ofMap (ks: 'k list) (vs: 'v list) : Map<'k, 'v> list =
    if List.isEmpty ks || List.isEmpty vs then
        failwith $"must not be empty: %s{nameof ofMap}"
        
    let kvs =
        List.collect
            (fun ks -> List.map (List.zip ks) (ofList vs (Checked.uint32 (List.length ks))))
            (ListEx.power ks) in

    List.map Map kvs
