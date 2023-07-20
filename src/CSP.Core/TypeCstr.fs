module CSP.Core.TypeCstr

open CSP.Core.Type
open CSP.Core.Var

type UnionNameCstr =
    | UNAny
    | UNName of string
    
type TCVarId = uint

type TypeCstr =
    | TCVar of TCVarId
    | TCUnit
    | TCNat
    | TCBool
    | TCTuple of TypeCstr * TypeCstr
    | TCSet of TypeCstr
    | TCList of TypeCstr
    | TCMap of TypeCstr * TypeCstr
    | TCUnion of UnionNameCstr * TypeCstr
    | TCError

type TypeEnv = Map<Var, TypeCstr>

let rec formatUnionName (un: UnionNameCstr) =
    match un with
    | UNAny -> "*"
    | UNName n -> n

let rec format (tc: TypeCstr) =
    match tc with
    | TCVar n -> $"'t%d{n}"
    | TCUnit -> "unit"
    | TCNat -> "nat"
    | TCBool -> "bool"
    | TCTuple(tcL, tcR) -> $"({format tcL} * {format tcR})"
    | TCSet(tc) -> $"({format tc} set)"
    | TCList(tc) -> $"({format tc} list)"
    | TCMap(tcK, tcV) -> $"(({format tcK}, {format tcV}) map)"
    | TCUnion(name, t) -> $"({format t} {formatUnionName name})"
    | TCError -> "error"

let merge (m1: Map<'K, 'V>) (m2: Map<'K, 'V>) : Map<'K, 'V> =
    Map.fold (fun acc k v -> Map.add k v acc) m1 m2

let rec ofType (t: Type) : TypeCstr =
    match t with
    | TVar n -> TCVar n
    | TUnit -> TCUnit
    | TNat -> TCNat
    | TBool -> TCBool
    | TTuple(tL, tR) -> TCTuple(ofType tL, ofType tR)
    | TSet(t) -> TCSet(ofType t)
    | TList(t) -> TCList(ofType t)
    | TMap(tK, tV) -> TCMap(ofType tK, ofType tV)
    | TUnion(name, t) -> TCUnion(UNName name, ofType t)
    | TError -> TCError