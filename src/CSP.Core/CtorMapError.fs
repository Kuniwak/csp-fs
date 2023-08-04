module CSP.Core.CtorMapError

open CSP.Core.Ctor
open CSP.Core.UnionMapError

type CtorMapError =
    | DuplicatedCtor of Set<Ctor>
    | NoSuchCtor of Ctor
    | UnionMapError of UnionMapError

let format (err: CtorMapError) : string =
    match err with
    | DuplicatedCtor(cs) ->
        let s = cs |> Seq.map Ctor.format |> String.concat ", " in $"duplicated constructor found: [%s{s}]"
    | NoSuchCtor(ctor) -> $"no such constructor: %s{Ctor.format ctor}"
    | UnionMapError(err) -> format err
