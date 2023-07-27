module CSP.Core.TypeInference

open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.TypeCstrEnv
open CSP.Core.TypeEnvError

type TypeInferState =
    { UncertainVarMap: TypeCstrUncertainVar.VarMap }

let newUncertainVarId (s: TypeInferState) : UncertainVarId * TypeInferState =
    let id, fam = TypeCstrUncertainVar.newId s.UncertainVarMap in (id, { s with UncertainVarMap = fam })

let bindUncertainVar (id: UncertainVarId) (tc: TypeCstr) (s: TypeInferState) : TypeInferState =
    let fum = TypeCstrUncertainVar.bind id tc s.UncertainVarMap in { s with UncertainVarMap = fum }

let resolveUncertainVar (id: UncertainVarId) (s: TypeInferState) : TypeCstr option =
    TypeCstrUncertainVar.resolve id s.UncertainVarMap

let init: TypeInferState = { UncertainVarMap = TypeCstrUncertainVar.init }

let generalize (t: Type) (s: TypeInferState) : TypeCstr * TypeInferState =
    let rec generalize t s m =
        match t with
        | TVar n ->
            match Map.tryFind n m with
            | Some n' -> (TCUncertain n', s, m)
            | None ->
                let n', s = newUncertainVarId s
                (TCUncertain n', s, Map.add n n' m)
        | TBool -> (TCBool, s, m)
        | TNat -> (TCNat, s, m)
        | TTuple(ts) ->
            let tcs, s, m =
                List.foldBack (fun t (tcs, s, m) -> let tc, s, m = generalize t s m in (tc :: tcs, s, m)) ts ([], s, m)

            (TCTuple tcs, s, m)
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
        | TSet tElem ->
            let tc, s, m = generalize tElem s m
            (TCSet tc, s, m)
        | TList tElem ->
            let tc, s, m = generalize tElem s m
            (TCList tc, s, m)
        | TMap(tK, tV) ->
            let tcK, s, m = generalize tK s m
            let tcV, s, m = generalize tV s m
            (TCMap(tcK, tcV), s, m)

    let tc, s, _ = generalize t s Map.empty
    (tc, s)

let generalizeAll (ts: Type list) (s: TypeInferState) : TypeCstr list * TypeInferState =
    List.foldBack (fun t (tcs, s) -> let tc, s = generalize t s in (tc :: tcs, s)) ts ([], s)

let from (xs: (string * Type) seq) : Result<TypeCstrEnv * TypeInferState, TypeEnvError> =
    let tcenvRes, s =
        TypeEnv.fold
            (fun (tcenvRes, s) var t ->
                let tc, s = generalize t s in (Result.bind (bind1 var tc) tcenvRes, s))
            (Ok(empty), init)
            (TypeEnv.from xs) in

    match tcenvRes with
    | Ok(tcenv) -> Ok(tcenv, s)
    | Error(terr) -> Error(terr)