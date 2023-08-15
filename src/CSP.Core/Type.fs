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
    | TUnion of UnionName * Type list

let format (t: Type) : string =
    let rec format t =
        match t with
        | TUnit -> "unit"
        | TVar n -> $"'t%d{n}"
        | TNat _ -> "nat"
        | TBool _ -> "bool"
        | TTuple(tL, tR) -> $"(tuple %s{format tL} %s{format tR})"
        | TSet(t) -> $"(set %s{format t})"
        | TList(t) -> $"(list %s{format t})"
        | TMap(tk, tv) -> $"(map %s{format tk} %s{format tv})"
        | TUnion(n, ts) ->
            if List.length ts = 0 then
                $"%s{n}"
            else
                let s = ts |> Seq.map format |> String.concat " "
                $"(%s{n} %s{s})"

    format t

let rec bind (u: TVarId) (t: Type) (target: Type) : Type =
    match target with
    | TVar u' when u = u' -> t
    | _ -> target

let bindByMap (m: Map<TVarId, Type>) (target: Type) : Type =
    Map.fold (fun target u t -> bind u t target) target m

let bindAll (u: TVarId) (t: Type) (ts: Type list) : Type list = List.map (bind u t) ts

let bindAllByMap (m: Map<TVarId, Type>) (ts: Type list) : Type list =
    Map.fold (fun ts u t -> bindAll u t ts) ts m
