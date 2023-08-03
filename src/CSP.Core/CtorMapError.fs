module CSP.Core.CtorMapError

open CSP.Core.Ctor
open CSP.Core.UnionMapError

type CtorMapError =
    | DuplicateCtor of Ctor
    | NoSuchCtor of Ctor
    | UnionMapError of UnionMapError

let format (err: CtorMapError) : string =
    match err with
    | DuplicateCtor(ctor) -> $"duplicated constructor found: %s{Ctor.format ctor}"
    | NoSuchCtor(ctor) -> $"no such constructor: %s{Ctor.format ctor}"
    | UnionMapError(err) -> format err
