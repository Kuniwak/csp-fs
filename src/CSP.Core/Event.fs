module CSP.Core.Event

open CSP.Core.Val

type Event<'Ctor when 'Ctor: comparison> =
    | Vis of Val<'Ctor>
    | Hid of Val<'Ctor>
    | Tau
    | Tick
    | Error
    
let format (ev: Event<'Ctor>) : string =
    match ev with
    | Vis ev' -> format ev'
    | Hid ev' -> $"τ ({format ev'})"
    | Tau -> "τ"
    | Tick -> "✓"
    | Error -> "ERROR"
