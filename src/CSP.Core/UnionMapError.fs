module CSP.Core.UnionMapError

open CSP.Core.Ctor
open CSP.Core.Type

type UnionMapError =
    | DuplicatedUnionName of Set<UnionName>
    | NoSuchUnion of UnionName
    | NoSuchCtor of UnionName * Ctor
    | TVarsLenMismatch of TVarId list * Type list

let format (err: UnionMapError) : string =
    match err with
    | DuplicatedUnionName(s) -> let s = s |> String.concat ", " in $"duplicated union name: [%s{s}]"
    | NoSuchUnion(un) -> $"no such union: %s{un}"
    | NoSuchCtor(un, ctor) -> $"no such constructor in %s{un}: %s{Ctor.format ctor}"
    | TVarsLenMismatch(tVars, ts) ->
        let s1 = tVars |> Seq.map (TVar >> format) |> String.concat " " in
        let s2 = ts |> Seq.map format |> String.concat " " in
        $"type variable length mismatch: (%s{s1}) vs (%s{s2})"
