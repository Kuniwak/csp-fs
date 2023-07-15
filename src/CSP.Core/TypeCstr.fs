module CSP.Core.TypeCstr

open CSP.Core.Ctor
open CSP.Core.Type
open CSP.Core.Val

type TypeCstr =
    | TCVar of uint
    | TCUnit
    | TCNat
    | TCBool
    | TCTuple of TypeCstr * TypeCstr
    | TCSet of TypeCstr
    | TCList of TypeCstr
    | TCMap of TypeCstr * TypeCstr
    | TCUnion of string * TypeCstr
    | TCEvent of TypeCstr
    | TCError

let rec format (tc: TypeCstr) =
    match tc with
    | TCVar n -> $"'t{n}"
    | TCUnit -> "unit"
    | TCNat -> "nat"
    | TCBool -> "bool"
    | TCTuple(tcL, tcR) -> $"({format tcL} * {format tcR})"
    | TCSet(tc) -> $"({format tc} set)"
    | TCList(tc) -> $"({format tc} list)"
    | TCMap(tcK, tcV) -> $"(({format tcK}, {format tcV}) map)"
    | TCUnion(name, t) -> $"({format t} {name})"
    | TCEvent t -> $"({format t} event)"
    | TCError -> "ERROR"

let merge (m1: Map<'K, 'V>) (m2: Map<'K, 'V>) : Map<'K, 'V> =
    Map.fold (fun acc k v -> Map.add k v acc) m1 m2

let rec ofType (t: Type) : TypeCstr =
    match t with
    | TUnit -> TCUnit
    | TNat -> TCNat
    | TBool -> TCBool
    | TTuple(tL, tR) -> TCTuple(ofType tL, ofType tR)
    | TSet(t) -> TCSet(ofType t)
    | TList(t) -> TCList(ofType t)
    | TMap(tK, tV) -> TCMap(ofType tK, ofType tV)
    | TUnion(name, t) -> TCUnion(name, ofType t)
    | TEvent t -> TCEvent(ofType t)
    | TError -> TCError

let ofVal
    (mc: Map<Ctor<'Ctor>, string * Type>)
    (acc: uint * (uint * TypeCstr) list)
    (v: Val<'Ctor>)
    : uint * (uint * TypeCstr) list =
    let rec ofVal acc v =
        match acc with
        | n, m ->
            match v with
            | VUnit -> n + 1u, (n, TCUnit) :: m
            | VNat _ -> n + 1u, (n, TCNat) :: m
            | VBool _ -> n + 1u, (n, TCBool) :: m
            | VTuple(vL, vR) ->
                match ofVal (n, m) vL with
                | nL, m ->
                    match ofVal (nL, m) vR with
                    | nR, m -> nR + 1u, (nR, TCTuple(TCVar(nL - 1u), TCVar(nR - 1u))) :: m
            | VSet(vs) ->
                let tcVar = TCVar n in
                let n = n + 1u in

                let acc =
                    Set.fold
                        (fun (n, m) v ->
                            match ofVal (n, m) v with
                            | n, m -> (n + 1u, (n, TCVar(n - 1u)) :: (n, tcVar) :: m))
                        (n, m)
                        vs in

                match acc with
                | n, m -> (n + 1u, (n, (TCSet tcVar)) :: m)
            | VList(vs) ->
                let tcVar = TCVar n in
                let n = n + 1u in

                let acc =
                    List.fold
                        (fun (n, m) v ->
                            match ofVal (n, m) v with
                            | n, m -> (n + 1u, (n, TCVar(n - 1u)) :: (n, tcVar) :: m))
                        (n, m)
                        vs in

                match acc with
                | n, m -> (n + 1u, (n, (TCList tcVar)) :: m)
            | VMap(vs) ->
                let tcVarK = TCVar n in
                let n = n + 1u in
                let tcVarV = TCVar n in
                let n = n + 1u in

                let acc =
                    Map.fold
                        (fun (n, m) k v ->
                            match ofVal (n, m) k with
                            | nK, m ->
                                let n = nK + 1u in
                                let m = (nK, TCVar(nK - 1u)) :: (nK, tcVarK) :: m in

                                match ofVal (n, m) v with
                                | nV, m ->
                                    let n = nV + 1u in
                                    let m = (nV, TCVar(nV - 1u)) :: (nV, tcVarV) :: m in
                                    (n, m))
                        (n, m)
                        vs in

                match acc with
                | n, m -> (n + 1u, (n, TCMap(tcVarK, tcVarV)) :: m)
            | VUnion(ctor, v) ->
                match ofVal (n, m) v with
                | nV, m ->
                    match Map.find ctor mc with
                    | name, t -> (nV + 2u, (nV + 1u, TCUnion(name, TCVar nV)) :: (nV, ofType t) :: (nV, TCVar (nV - 1u)) :: m)
            | VEvent v ->
                let tcVar = TCVar n in
                let n = n + 1u in
                match ofVal (n, m) v with
                | nV, m -> (nV + 2u, (nV+1u, TCEvent(tcVar)) :: (nV, tcVar) :: (nV, TCVar (nV - 1u)) :: m)
            | VError -> (n + 1u, (n, TCError) :: m)

    ofVal acc v
