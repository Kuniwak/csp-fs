module CSP.Core.CtorMap

open CSP.Core.Ctor
open CSP.Core.Type
open CSP.Core.UnionMap
open CSP.Core.UnionMapError
open CSP.Core.CtorMapError
open CSP.Core.Util

type CtorMap = CtorMap of Map<Ctor, UnionName>

let toSeq (cm: CtorMap) : (Ctor * UnionName) seq =
    match cm with
    | CtorMap cm -> Map.toSeq cm

let from (um: UnionMap) : Result<CtorMap, CtorMapError> =
    um
    |> UnionMap.toSeq
    |> Seq.collect (fun (un, (_, cm)) -> cm |> Map.keys |> Seq.map (fun ctor -> (ctor, un)))
    |> MapEx.tryFrom
    |> Result.map CtorMap
    |> Result.mapError DuplicatedCtor


let tryFindOnlyUnionName (ctor: Ctor) (cm: CtorMap) : Result<UnionName, CtorMapError> =
    match cm with
    | CtorMap cm ->
        match Map.tryFind ctor cm with
        | None -> Error(NoSuchCtor ctor)
        | Some(un) -> Ok(un)

let tryFindAssocLen (ctor: Ctor) (um: UnionMap) (cm: CtorMap) : Result<int, CtorMapError> =
    tryFindOnlyUnionName ctor cm
    |> Result.bind (fun un -> tryFindAssocLen un ctor um |> Result.mapError UnionMapError)

let tryFind (ctor: Ctor) (um: UnionMap) (cm: CtorMap) : Result<UnionName * TVarId list * Type list, CtorMapError> =
    tryFindOnlyUnionName ctor cm
    |> Result.bind (fun un ->
        tryFind un um
        |> Result.mapError UnionMapError
        |> Result.bind (fun (tVars, cm) ->
            match Map.tryFind ctor cm with
            | None -> Error(NoSuchCtor(ctor))
            | Some(ts) -> Ok(un, tVars, ts)))

let toCtorTypes
    (ctor: Ctor)
    (um: UnionMap)
    (unionTs: Type list)
    (cm: CtorMap)
    : Result<UnionName * Type list, CtorMapError> =
    tryFindOnlyUnionName ctor cm
    |> Result.bind (fun un ->
        UnionMap.tryFind un um
        |> Result.bind (fun (tVars, cm) ->
            if List.length tVars = List.length unionTs then
                Ok(List.zip tVars unionTs, cm)
            else
                Error(TVarsLenMismatch(tVars, unionTs)))
        |> Result.mapError UnionMapError
        |> Result.bind (fun (tVarTs, cm) ->
            match Map.tryFind ctor cm with
            | None -> Error(NoSuchCtor ctor)
            | Some(ts) -> Ok(un, bindAllByMap (Map tVarTs) ts)))


let fold (f: 'State -> Ctor -> UnionName -> 'State) (s: 'State) (cm: CtorMap) : 'State =
    match cm with
    | CtorMap cm -> Map.fold f s cm
