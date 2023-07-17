module CSP.Core.Type

type UnionName = string

type Type =
    | TVar of uint
    | TUnit
    | TNat
    | TBool
    | TTuple of Type * Type
    | TSet of Type
    | TList of Type
    | TMap of Type * Type
    | TUnion of string * Type
    | TError

let tOption (t: Type) = TUnion("option", t)

let tEither (tl: Type) (tr: Type) = TUnion("either", TTuple(tl, tr))

let tTriple (t1: Type) (t2: Type) (t3: Type) = TTuple(t1, TTuple(t2, t3))

let rec format (t: Type) : string =
    match t with
    | TVar n -> $"'t{n}"
    | TUnit -> "unit"
    | TNat -> "nat"
    | TBool -> "bool"
    | TTuple(lt, rt) -> $"({format lt} * {format rt})"
    | TSet t -> $"({format t} set)"
    | TList t -> $"({format t} list)"
    | TMap(tk, tv) -> $"(({format tk}, {format tv}) map)"
    | TUnion(n, t) -> $"({format t} {n})"
    | TError -> "error"
