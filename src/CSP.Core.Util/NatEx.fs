module CSP.Core.Util.NatEx

open System

let tryParse (s: string): uint option =
    let mutable result = 0u in
    if UInt32.TryParse(s, &result) then
        Some(result)
    else
        None