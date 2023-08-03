module CSP.Core.UnionMap

open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Type
open CSP.Core.UnionMapError

type UnionMap = UnionMap of Map<UnionName, TVarId list * Map<Ctor, Type list>>

let tryFind (un: UnionName) (um: UnionMap) : Result<TVarId list * Map<Ctor, Type list>, UnionMapError> =
    match um with
    | UnionMap um ->
        match Map.tryFind un um with
        | None -> Error(NoSuchUnion un)
        | Some(cm) -> Ok(cm)

let tryFindAssocLen (un: UnionName) (ctor: Ctor) (um: UnionMap) : Result<int, UnionMapError> =
    tryFind un um
    |> Result.bind (fun (_, cm) ->
        match Map.tryFind ctor cm with
        | None -> Error(NoSuchCtor ctor)
        | Some(ts) -> Ok(List.length ts))

let instantiateCtorMap (un: UnionName) (tm: Map<TVarId, Type>) (um: UnionMap) : Result<Map<Ctor, Type list>, UnionMapError> =
    tryFind un um
    |> Result.map (fun (tVars, cm) ->
        Map.fold
            (fun cm ctor ts ->
                let ts =
                    instantiateListByMap (Map.filter (fun tVar _ -> List.contains tVar tVars) tm) ts in

                Map.add ctor ts cm)
            Map.empty
            cm)

let empty: UnionMap = UnionMap Map.empty
