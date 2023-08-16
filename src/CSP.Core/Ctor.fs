module CSP.Core.Ctor

type Ctor = Ctor of string

let format (ctor: Ctor) : string =
    match ctor with
    | Ctor n -> n
    
let formatOpt (ctorOpt: Ctor option) : string =
    ctorOpt
    |> Option.map format
    |> Option.defaultValue "_"
