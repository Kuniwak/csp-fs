module CSP.Core.Univ

open CSP.Core.Util
open CSP.Core.UnivError
open CSP.Core.Ctor
open CSP.Core.Type
open CSP.Core.Val

let rec rangeVNat (n1: uint32) (n2: uint32) : Set<Val> = Set.map VNat (Range.ofSet n1 n2)
type UnivConfig = { NatMax: uint; ListLenMax: uint }

let univConfig natMax listLenMax : UnivConfig =
    { NatMax = natMax
      ListLenMax = listLenMax }

let univ (cfg: UnivConfig) (t: Type) : Result<Val list, UnivError> =
    let rec univ t =
        match t with
        | TVar _ -> Error(UnivTVarIsNotAllowed)
        | TNat _ -> Ok(List.map VNat (Range.ofList 0u cfg.NatMax))
        | TBool _ -> Ok([ VBool false; VBool true ])
        | TTuple(ts, _) ->
            let vssRes =
                List.foldBack
                    (fun t vssRes ->
                        match vssRes, univ t with
                        | Ok(vss), Ok(vs) -> Ok(vs :: vss)
                        | Error err, _ -> Error err
                        | _, Error err -> Error err)
                    ts
                    (Ok([]))

            Result.map (ListEx.cartesian >> List.map VTuple) vssRes
        | TSet(t, _) -> Result.map (Set.ofList >> Univ.ofSet >> List.map VSet) (univ t)
        | TList(t, _) ->
            Result.map
                (fun vs ->
                    List.map
                        VList
                        (List.collect (fun listLen -> Univ.ofList listLen vs) (Range.ofList 0u cfg.ListLenMax)))
                (univ t)
        | TMap(tk, tv, _) ->
            match univ tk, univ tv with
            | Ok(ks), Ok(vs) -> Ok(List.map VMap (Univ.ofMap ks vs))
            | Error(err), _ -> Error(err)
            | _, Error(err) -> Error(err)

        | TUnion(_, cm, _) ->
            let cvsListRes: Result<(Ctor * Val list) list, UnivError> =
                List.fold
                    (fun cvsListRes (ctor, ts) ->
                        Result.bind
                            (fun cvsList ->
                                let vssRes: Result<Val list list, UnivError> =
                                    List.foldBack
                                        (fun t ->
                                            Result.bind (fun vss ->
                                                Result.map
                                                    (fun vs ->
                                                        List.map
                                                            (fun (v, vs) -> v :: vs)
                                                            (ListEx.cartesian2 vs vss))
                                                    (univ t)))
                                        ts
                                        (Ok []) in

                                Result.map
                                    (fun cvsList' -> cvsList @ cvsList')
                                    (Result.map (List.map (fun vs -> (ctor, vs))) vssRes))
                            cvsListRes)
                    (Ok([]))
                    (Map.toList cm) in

            Result.map (List.map VUnion) cvsListRes

    univ t
