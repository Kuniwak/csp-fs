module CSP.Core.EventSpec

type EventSpec<'Ev, 'Ch when 'Ev: comparison and 'Ch: comparison> =
    | Event of 'Ev
    | Chan of 'Ch

let format (ess: Set<EventSpec<'Ev, 'Ch>>) : string =
    let onlyEvs = List.sort (List.collect (fun ev -> match ev with Event e -> [$"{e}"] | Chan _ -> []) (Set.toList ess))
    let onlyChans = List.sort (List.collect (fun ev -> match ev with Event e -> [] | Chan ch -> [$"{ch}"]) (Set.toList ess))
    let sep = ", "
    match (List.isEmpty onlyEvs, List.isEmpty onlyChans) with
    | true, true -> "{}"
    | true, false -> $"⦃{String.concat sep onlyChans}⦄"
    | false, true -> $"{{{String.concat sep onlyEvs}}}"
    | false, false -> $"{{{String.concat sep onlyEvs}}} ∪ ⦃{String.concat sep onlyChans}⦄"
