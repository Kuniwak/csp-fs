module CSP.Core.Type

open CSP.Core.Ctor

type UnionName = string

type TypeClassName = string
type TVarId = uint

// TODO: Introduce Kind
type Type =
    | TVar of TVarId
    | TNat
    | TBool
    | TTuple of Type list
    | TSet of Type
    | TList of Type
    | TMap of Type * Type
    | TUnion of UnionName * Map<Ctor, Type list>

let rec format (t: Type) : string =
    match t with
    | TVar n -> $"'t%d{n}"
    | TNat -> "nat"
    | TBool -> "bool"
    | TTuple ts -> let s = String.concat " * " (List.map format ts) in $"(%s{s})"
    | TSet t -> $"(%s{format t} set)"
    | TList t -> $"(%s{format t} list)"
    | TMap(tk, tv) -> $"((%s{format tk}, %s{format tv}) map)"
    | TUnion(n, cm) ->
        let s =
            String.concat
                "/"
                (List.map
                    (fun (ctor, ts) -> let s = String.concat ", " (List.map format ts) in $"{Ctor.format ctor} [%s{s}]")
                    (Map.toList cm)) in

        $"(%s{n} %s{s})"
