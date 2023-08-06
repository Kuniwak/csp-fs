module CSP.Core.UnionMap

open CSP.Core.Ctor
open CSP.Core.Type
open CSP.Core.UnionMapError
open CSP.Core.Util

type UnionMap = UnionMap of Map<UnionName, TVarId list * Map<Ctor, Type list>>

let builtin =
    UnionMap(
        Map
            [ ("option", ([ 0u ], Map [ (Ctor "Some", [ TVar 0u ]); (Ctor "None", []) ]))
              ("either", ([ 0u; 1u ], Map [ (Ctor "Left", [ TVar 0u ]); (Ctor "Right", [ TVar 1u ]) ])) ]
    )

let toSeq (um: UnionMap) : (UnionName * (TVarId list * Map<Ctor, Type list>)) seq =
    match um with
    | UnionMap um -> Map.toSeq um

let from (xs: ((TVarId list * UnionName) * (string * Type list) seq) seq) : Result<UnionMap, UnionMapError> =
    xs
    |> Seq.map (fun ((tVars, un), cm) -> (un, (tVars, Map(Seq.map (fun (ctor, ts) -> (Ctor ctor, ts)) cm))))
    |> Seq.append (toSeq builtin)
    |> MapEx.tryFrom
    |> Result.map UnionMap
    |> Result.mapError DuplicatedUnionName

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
        | None -> Error(NoSuchCtor(un, ctor))
        | Some(ts) -> Ok(List.length ts))
    
let instantiateCtorMap (un: UnionName) (ts: Type list) (um: UnionMap) : Result<Map<Ctor, Type list>, UnionMapError> =
    tryFind un um
    |> Result.bind (fun (tVars, cm) ->
        if List.length tVars = List.length ts then
            Ok((List.zip tVars ts), cm)
        else
            Error(TVarsLenMismatch(tVars, ts)))
    |> Result.map (fun (tVarTs, cm) ->
        Map.fold (fun cm ctor ts -> Map.add ctor (bindAllByMap (Map tVarTs) ts) cm) Map.empty cm)

let fold (f: 'State -> UnionName -> TVarId list * Map<Ctor, Type list> -> 'State) (s: 'State) (um: UnionMap) : 'State =
    match um with
    | UnionMap um -> Map.fold f s um
