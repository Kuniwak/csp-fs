module CSP.Core.CtorMapError

open CSP.Core.Ctor

type CtorMapError = DuplicateCtor of Ctor

let format (err: CtorMapError) : string =
    match err with
    | DuplicateCtor(ctor) -> $"duplicated constructor found: %s{format ctor}"
