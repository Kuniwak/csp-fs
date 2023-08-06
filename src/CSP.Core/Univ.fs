module CSP.Core.Univ

open CSP.Core.UnionMap
open CSP.Core.Util
open CSP.Core.UnivError
open CSP.Core.Type
open CSP.Core.Val

let rec rangeVNat (n1: uint32) (n2: uint32) : Set<Val> = Set.map VNat (Range.ofSet n1 n2)
type UnivConfig = { NatMax: uint; ListLenMax: uint }

let univConfig natMax listLenMax : UnivConfig =
    { NatMax = natMax
      ListLenMax = listLenMax }

let univ (cfg: UnivConfig) (um: UnionMap) (t: Type) : Result<Val list, UnivError> =
    let rec univ t =
        match t with
        | TUnit _ -> Ok([ VUnit ])
        | TVar _ -> Error(UnivTVarIsNotAllowed)
        | TNat _ -> Ok(List.map VNat (Range.ofList 0u cfg.NatMax))
        | TBool _ -> Ok([ VBool false; VBool true ])
        | TTuple(tL, tR) ->
            tL
            |> univ
            |> Result.bind (fun ls -> tR |> univ |> Result.map (ListEx.cartesian2 ls) |> Result.map (List.map VTuple))
        | TSet(t) -> t |> univ |> Result.map (Set.ofList >> Univ.ofSet >> List.map VSet)
        | TList(t) ->
            t
            |> univ
            |> Result.map (fun vs ->
                Range.ofList 0u (cfg.ListLenMax + 1u)
                |> List.collect (Univ.ofList vs)
                |> List.map VList)
        | TMap(tk, tv) ->
            Result.bind (fun ks -> Result.map (fun vs -> List.map VMap (Univ.ofMap ks vs)) (univ tv)) (univ tk)

        | TUnion(un, ts) ->
            instantiateCtorMap un ts um
            |> Result.mapError UnionMapError
            |> Result.bind (fun cm ->
                cm
                |> Map.toList
                |> ResultEx.bindAll (fun (ctor, ts) ->
                    ts
                    |> ResultEx.bindAll univ
                    |> Result.map ListEx.cartesian
                    |> Result.map (List.map (fun vs -> VUnion(ctor, vs))))
                |> Result.map List.concat)

    univ t
