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
    | VUnion of Ctor * Val list


let rec format (v: Val) : string =
    match v with
    | VUnit -> "()"
    | VNat n -> $"%d{n}"
    | VBool b -> $"%b{b}"
    | VTuple(vL, vR) -> $"(%s{format vL}, %s{format vR})"
    | VSet s -> let s' = String.concat ", " (List.map format (Set.toList s)) in $"{{%s{s'}}}"
    | VList vs -> let s' = String.concat ", " (List.map format vs) in $"[%s{s'}]"
    | VMap m ->
        let s' =
            String.concat ", " (List.map (fun (k, v) -> $"%s{format k}: %s{format v}") (Map.toList m)) in

        $"{{%s{s'}}}"
    | VUnion(ctor, vs) ->
        match List.length vs with
        | 0 -> Ctor.format ctor
        | 1 -> $"(%s{Ctor.format ctor} %s{format vs[0]})"
        | _ -> let s = String.concat " " (List.map format vs) in $"(%s{Ctor.format ctor} %s{s})"
