module CSP.Core.Type

open CSP.Core.Ctor

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
    | TUnion of UnionName * Map<Ctor, Type list>

let rec format (t: Type) : string =
    match t with
    | TUnit -> "()"
    | TVar n -> $"'t%d{n}"
    | TNat _ -> "nat"
    | TBool _ -> "bool"
    | TTuple(tL, tR) -> $"(%s{format tL}, %s{format tR})"
    | TSet(t) -> $"(%s{format t} set)"
    | TList(t) -> $"(%s{format t} list)"
    | TMap(tk, tv) -> $"((%s{format tk}, %s{format tv}) map)"
    | TUnion(n, cm) ->
        let s =
            String.concat
                "/"
                (List.map
                    (fun (ctor, ts) -> let s = String.concat ", " (List.map format ts) in $"{Ctor.format ctor} [%s{s}]")
                    (Map.toList cm)) in

        $"(%s{n} %s{s})"
