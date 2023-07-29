module CSP.Core.TypeInferenceState

open CSP.Core.Ctor
open CSP.Core.Type
open CSP.Core.TypeCstr

type State =
    { UncertainVarMap: TypeCstrUncertainVar.VarMap }

let newUncertainVarId (s: State) : UncertainVarId * State =
    let id, fam = TypeCstrUncertainVar.newId s.UncertainVarMap in (id, { s with UncertainVarMap = fam })

let bindUncertainVar (id: UncertainVarId) (tc: TypeCstr) (s: State) : State =
    let fum = TypeCstrUncertainVar.bind id tc s.UncertainVarMap in { s with UncertainVarMap = fum }

let resolveUncertainVar (id: UncertainVarId) (s: State) : TypeCstr option =
    TypeCstrUncertainVar.resolve id s.UncertainVarMap

let init: State = { UncertainVarMap = TypeCstrUncertainVar.init }

let generalize (t: Type) (s: State) : TypeCstr * State =
    let rec generalize t s m =
        match t with
        | TUnit _ -> (TCUnit, s, m)
        | TVar(n) ->
            match Map.tryFind n m with
            | Some n' -> (TCUncertain n', s, m)
            | None ->
                let n', s = newUncertainVarId s
                (TCUncertain n', s, Map.add n n' m)
        | TBool _ -> (TCBool, s, m)
        | TNat _ -> (TCNat, s, m)
        | TTuple(tL, tR) ->
            let tcL, s, m = generalize tL s m in
            let tcR, s, m = generalize tR s m in
            (TCTuple(tcL, tcR), s, m)
        | TUnion(un, cm) ->
            let cm, s, m =
                Map.fold
                    (fun (cm, s, m) ctor ts ->
                        let tcs, s, m =
                            List.foldBack
                                (fun t (tcs, s, m) -> let tc, s, m = generalize t s m in (tc :: tcs, s, m))
                                ts
                                ([], s, m) in

                        let cm = Map.add ctor tcs cm in

                        (cm, s, m))
                    (Map.empty, s, m)
                    cm in

            (TCUnion(un, cm), s, m)
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

    let tc, s, _ = generalize t s Map.empty
    (tc, s)

let generalizeList (ts: Type list) (s: State) : TypeCstr list * State =
    List.foldBack (fun t (tcs, s) -> let tc, s = generalize t s in (tc :: tcs, s)) ts ([], s)

let generalizeMap (m: Map<Ctor, Type list>) (s: State) : Map<Ctor, TypeCstr list> * State =
    Map.fold (fun (m, s) ctor ts -> let tcs, s = generalizeList ts s in (Map.add ctor tcs m, s)) (Map.empty, s) m