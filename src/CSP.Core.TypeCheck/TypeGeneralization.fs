module CSP.Core.TypeGeneralization

open CSP.Core
open CSP.Core.TypeError
open CSP.Core.TypeInferenceState
open CSP.Core.Ctor
open CSP.Core.CtorMap
open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.UnionMap

let rec generalize (t: Type) (s: State) m : TypeCstr * State * Map<TVarId, UncertainVarId> =
    match t with
    | TUnit -> (TCUnit, s, m)
    | TVar(n) ->
        match Map.tryFind n m with
        | Some n' -> (TCUncertain n', s, m)
        | None ->
            let n', s = newUncertainVarId s
            (TCUncertain n', s, Map.add n n' m)
    | TBool -> (TCBool, s, m)
    | TNat -> (TCNat, s, m)
    | TTuple(tL, tR) ->
        let tcL, s, m = generalize tL s m in
        let tcR, s, m = generalize tR s m in
        (TCTuple(tcL, tcR), s, m)
    | TUnion(un, ts) ->
        let tcs, s, m =
            List.foldBack (fun t (tcs, s, m) -> let tc, s, m = generalize t s m in (tc :: tcs, s, m)) ts ([], s, m) in

        (TCUnion(un, tcs), s, m)
    | TSet(tElem) ->
        let tc, s, m = generalize tElem s m
        (TCSet tc, s, m)
    | TList(tElem) ->
        let tc, s, m = generalize tElem s m
        (TCList tc, s, m)
    | TMap(tK, tV) ->
        let tcK, s, m = generalize tK s m
        let tcV, s, m = generalize tV s m
        (TCMap(tcK, tcV), s, m)

let generalizeList
    (ts: Type list)
    (s: State)
    (m: Map<TVarId, UncertainVarId>)
    : TypeCstr list * State * Map<TVarId, UncertainVarId> =
    List.foldBack (fun t (tcs, s, m) -> let tc, s, m = generalize t s m in (tc :: tcs, s, m)) ts ([], s, m)

let generalizeMap
    (tsm: Map<Ctor, Type list>)
    (tVars: TVarId list)
    (s: State)
    (m: Map<TVarId, UncertainVarId>)
    : TypeCstr list * Map<Ctor, TypeCstr list> * State * Map<TVarId, UncertainVarId> =
    let tcm, s, m =
        Map.fold
            (fun (tsm, s, m) ctor ts -> let tcs, s, m = generalizeList ts s m in (Map.add ctor tcs tsm, s, m))
            (Map.empty, s, m)
            tsm in

    let tcs, s, m =
        List.foldBack
            (fun tVar (tcs, s, m) ->
                match Map.tryFind tVar m with
                | Some(u) -> (TCUncertain u :: tcs, s, m)
                | None -> let u, s = newUncertainVarId s in (TCUncertain u :: tcs, s, Map.add tVar u m))
            tVars
            ([], s, m)

    (tcs, tcm, s, m)

let generalizeUnion
    (um: UnionMap)
    (cm: CtorMap)
    (ctor: Ctor)
    (ctorTcs: TypeCstr list)
    (s: State)
    : Result<TypeCstr * State, TypeError> =
    CtorMap.tryFind ctor um cm
    |> Result.mapError CtorMapError
    |> Result.bind (fun (un, tVars, ctorTs) ->
        if List.length ctorTcs = List.length ctorTs then
            List.zip ctorTcs ctorTs
            |> List.fold
                (fun mRes (ctorTC, ctorT) ->
                    mRes
                    |> Result.bind (fun m ->
                        match ctorT with
                        | TVar n ->
                            if Map.containsKey n m then
                                let ctorTC' = Map.find n m in

                                if ctorTC = ctorTC' then
                                    Ok(m)
                                else
                                    Error(
                                        TypeMismatch(
                                            Set[ctorTC
                                                ctorTC']
                                        )
                                    )
                            else
                                Ok(Map.add n ctorTC m)
                        | _ -> Ok(m)))
                (Ok(Map.empty))
            |> Result.bind (fun m ->
                let tcs, s =
                    List.foldBack
                        (fun tVar (tcs, s) ->
                            let u, s = newUncertainVarId s in
                            ((Map.tryFind tVar m |> Option.defaultValue (TCUncertain u)) :: tcs, s))
                        tVars
                        ([], s) in

                Ok(TCUnion(un, tcs), s))
        else
            Error(
                AssociatedValuesLenMismatch(
                    ctor,
                    Set[List.length ctorTs
                        List.length ctorTcs]
                )
            ))
