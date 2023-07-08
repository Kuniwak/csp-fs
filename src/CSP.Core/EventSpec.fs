module CSP.Core.EventSpec

type EventSpec<'Ev, 'Ch when 'Ev: comparison and 'Ch: comparison> =
    | Event of 'Ev
    | Chan of 'Ch

let format (evs: Set<EventSpec<'Ev, 'Ch>>) : string =
    let evs' = List.sort (List.map (fun ev -> match ev with Event e -> $"{e}" | Chan ch -> $"{ch}.*") (Set.toList evs)) in
    let sep = ", "
    $"{{{String.concat sep evs'}}}"
