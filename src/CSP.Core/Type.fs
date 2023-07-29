module CSP.Core.Type

open CSP.Core.Ctor
open CSP.Core.LineNum

type UnionName = string

type TypeClassName = string
type TVarId = uint

type Type =
    | TVar of TVarId * LineNum
    | TNat of LineNum
    | TBool of LineNum
    | TTuple of Type list * LineNum
    | TSet of Type * LineNum
    | TList of Type * LineNum
    | TMap of Type * Type * LineNum
    | TUnion of UnionName * Map<Ctor, Type list> * LineNum

let rec format (t: Type) : string =
    match t with
    | TVar(n, _) -> $"'t%d{n}"
    | TNat _ -> "nat"
    | TBool _ -> "bool"
    | TTuple(ts, _) -> let s = String.concat " * " (List.map format ts) in $"(%s{s})"
    | TSet(t, _) -> $"(%s{format t} set)"
    | TList(t, _) -> $"(%s{format t} list)"
    | TMap(tk, tv, _) -> $"((%s{format tk}, %s{format tv}) map)"
    | TUnion(n, cm, _) ->
        let s =
            String.concat
                "/"
                (List.map
                    (fun (ctor, ts) -> let s = String.concat ", " (List.map format ts) in $"{Ctor.format ctor} [%s{s}]")
                    (Map.toList cm)) in

        $"(%s{n} %s{s})"
