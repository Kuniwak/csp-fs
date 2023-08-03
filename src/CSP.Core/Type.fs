module CSP.Core.Type

type UnionName = string

type TypeClassName = string
type TVarId = uint

type Type =
    | TUnit
    | TVar of TVarId
    | TNat
    | TBool
    // NOTE: `TTuple of Type * Type` is familiar to type inference instead of `TTuple of Type list.`
    //       `TupleNth()` is needed if `TTuple` take `Type list`. Then, inferring a type of `TupleNth(VarRef "x")` is hard if "x" typed as ?x.
    | TTuple of Type * Type
    | TSet of Type
    | TList of Type
    | TMap of Type * Type
    | TUnion of UnionName * Map<TVarId, Type>

let format (t: Type) : string =
    let rec format t =
        match t with
        | TUnit -> "()"
        | TVar n -> $"'t%d{n}"
        | TNat _ -> "nat"
        | TBool _ -> "bool"
        | TTuple(tL, tR) -> $"(%s{format tL}, %s{format tR})"
        | TSet(t) -> $"(%s{format t} set)"
        | TList(t) -> $"(%s{format t} list)"
        | TMap(tk, tv) -> $"((%s{format tk}, %s{format tv}) map)"
        | TUnion(n, ts) ->
            let s = ts |> Map.toSeq |> Seq.map snd |> Seq.map format |> String.concat " "
            $"(%s{n} %s{s})"

    format t

let rec instantiate (u: TVarId) (t: Type) (target: Type) : Type =
    match target with
    | TVar u' when u = u' -> t
    | _ -> target

let instantiateByMap (m: Map<TVarId, Type>) (target: Type) : Type =
    Map.fold (fun target u t -> instantiate u t target) target m

let instantiateList (u: TVarId) (t: Type) (ts: Type list) : Type list = List.map (instantiate u t) ts

let instantiateListByMap (m: Map<TVarId, Type>) (ts: Type list) : Type list =
    Map.fold (fun ts u t -> instantiateList u t ts) ts m
