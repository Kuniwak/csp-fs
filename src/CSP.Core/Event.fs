module CSP.Core.Event

open CSP.Core.Val

type Event<'Ev, 'Ch, 'Var, 'Ctor when 'Ctor: comparison> =
    | Vis of 'Ev
    | Hid of 'Ev
    | VisChan of 'Ch * Val<'Ctor>
    | HidChan of 'Ch * Val<'Ctor>
    | Tau
    | Tick
    | Error

let format (ev: Event<'Ev, 'Ch, 'Var, 'Ctor>) : string =
    match ev with
    | Vis ev' -> $"{ev'}"
    | Hid ev' -> $"τ ({ev'})"
    | VisChan(ch, v) -> $"{ch}.{format v}"
    | HidChan(ch, v) -> $"τ ({ch}.{format v})"
    | Tau -> "τ"
    | Tick -> "✓"
    | Error -> "ERROR"
