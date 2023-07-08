module CSP.Core.Event

open CSP.Core.Val

type Event<'Ev, 'Ch, 'Var, 'Ctor when 'Ctor: comparison> =
    | Vis of 'Ev
    | Hid of 'Ev
    | VisSend of 'Ch * Val<'Ctor>
    | HidSend of 'Ch * Val<'Ctor>
    | VisRecv of 'Ch * 'Var
    | HidRecv of 'Ch * 'Var
    | VisChan of 'Ch * Val<'Ctor>
    | HidChan of 'Ch * Val<'Ctor>
    | Tau
    | Tick
    | Error

let format (ev: Event<'Ev, 'Ch, 'Var, 'Ctor>) : string =
    match ev with
    | Vis ev' -> $"{ev'}"
    | Hid ev' -> $"τ ({ev'})"
    | VisSend(ch, v) -> $"{ch}!{v}"
    | HidSend(ch, v) -> $"τ ({ch}!{v})"
    | VisRecv(ch, v) -> $"{ch}?{v}"
    | HidRecv(ch, v) -> $"τ ({ch}?{v})"
    | VisChan(ch, v) -> $"{ch}.{v}"
    | HidChan(ch, v) -> $"τ ({ch}.{v})"
    | Tau -> "τ"
    | Tick -> "✓"
    | Error -> "ERROR"