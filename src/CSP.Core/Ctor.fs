module CSP.Core.Ctor

type Ctor = Ctor of string

let format (ctor: Ctor): string =
    match ctor with
    | Ctor n -> n
