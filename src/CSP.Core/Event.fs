module CSP.Core.Event

open CSP.Core.Val

type Event =
    | Vis of Val
    | Hid of Val
    | Tau
    | Tick
    | Error
    
let format (ev: Event) : string =
    match ev with
    | Vis ev' -> format ev'
    | Hid ev' -> $"τ ({format ev'})"
    | Tau -> "τ"
    | Tick -> "✓"
    | Error -> "ERROR"
