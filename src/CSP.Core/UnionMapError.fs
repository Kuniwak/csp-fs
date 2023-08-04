module CSP.Core.UnionMapError

open CSP.Core.Ctor
open CSP.Core.Type

type UnionMapError =
    | DuplicatedUnionName of Set<UnionName>
    | NoSuchUnion of UnionName
    | NoSuchCtor of Ctor

let format (err: UnionMapError) : string =
    match err with
    | DuplicatedUnionName(s) -> let s = s |> String.concat ", " in $"duplicated union name: [%s{s}]"
    | NoSuchUnion(un) -> $"no such union: %s{un}"
    | NoSuchCtor(ctor) -> $"no such constructor: %s{Ctor.format ctor}"
