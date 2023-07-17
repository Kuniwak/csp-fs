module CSP.Core.Val

open CSP.Core.Ctor


type Val =
    | VUnit
    | VNat of uint
    | VBool of bool
    | VTuple of Val * Val
    | VSet of Set<Val>
    | VList of Val list
    | VMap of Map<Val, Val>
    | VUnion of Ctor * Val
    | VError


let rec format (v: Val) : string =
    match v with
    | VUnit -> "()"
    | VNat n -> $"{n}"
    | VBool b -> $"{b}"
    | VTuple(l, r) -> $"({format l}, {format r})"
    | VSet(s) -> let s' = String.concat ", " (List.map format (Set.toList s)) in $"{{{s'}}}"
    | VList(vs) -> let s' = String.concat ", " (List.map format vs) in $"[{s'}]"
    | VMap(m) ->
        let s' =
            String.concat ", " (List.map (fun (k, v) -> $"{format k}: {format v}") (Map.toList m)) in

        $"{{{s'}}}"
    | VUnion(c, v) ->
        match c with
        | Ctor c' -> if v = VUnit then $"{c'}" else $"({c'} {format v})"
    | VError -> "ERROR"
