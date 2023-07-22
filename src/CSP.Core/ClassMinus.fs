module CSP.Core.ClassMinus

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Plus"

let derivedBy (t: Type) : bool =
    match t with
    | TNat -> true
    | TSet _ -> true
    | _ -> false

let minus (v1: Val) (v2: Val) : Val =
    match v1, v2 with
    | VNat n1, VNat n2 -> VNat(n1 - n2)
    | VSet n1, VSet n2 -> VSet(Set.difference n1 n2)
    | _ -> failwith $"cannot minus: %s{format v1} vs %s{format v2}"
