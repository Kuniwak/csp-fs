module CSP.Core.CtorMap

open CSP.Core.Ctor
open CSP.Core.CtorMapError
open CSP.Core.Type
open CSP.Core.UnionMap

type CtorMap = CtorMap of Map<Ctor, UnionName>

let tryFind (ctor: Ctor) (cm: CtorMap) : Result<UnionName, CtorMapError> =
    match cm with
    | CtorMap cm ->
        match Map.tryFind ctor cm with
        | None -> Error(NoSuchCtor ctor)
        | Some(un) -> Ok(un)

let tryFindAssocLen (ctor: Ctor) (um: UnionMap) (cm: CtorMap) : Result<int, CtorMapError> =
    tryFind ctor cm
    |> Result.bind (fun un -> tryFindAssocLen un ctor um |> Result.mapError UnionMapError)

let instantiate (ctor: Ctor) (um: UnionMap) (tm: Map<TVarId, Type>) (cm: CtorMap) : Result<Type list, CtorMapError> =
    tryFind ctor cm
    |> Result.bind (fun un ->
        UnionMap.tryFind un um
        |> Result.mapError UnionMapError
        |> Result.bind (fun (tVars, cm) ->
            match Map.tryFind ctor cm with
            | None -> Error(NoSuchCtor ctor)
            | Some(ts) -> Ok(instantiateListByMap (Map.filter (fun tVar _ -> List.contains tVar tVars) tm) ts)))

let fold (f: 'State -> Ctor -> UnionName -> 'State) (s: 'State) (cm: CtorMap) : 'State =
    match cm with
    | CtorMap cm -> Map.fold f s cm

let empty: CtorMap = CtorMap Map.empty
