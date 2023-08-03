module CSP.Core.UnionMapError

open CSP.Core.Ctor
open CSP.Core.Type

type UnionMapError =
    | NoSuchUnion of UnionName
    | NoSuchCtor of Ctor

let format (err: UnionMapError) : string =
    match err with
    | NoSuchUnion(un) -> $"no such union: %s{un}"
    | NoSuchCtor(ctor) -> $"no such constructor: %s{Ctor.format ctor}"
