module CSP.Core.ClassTimes

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Times"

let derivedBy (t: Type) : bool =
    match t with
    | TBool _ -> true
    | TNat _ -> true
    | TSet _ -> true
    | _ -> false

let times (v1: Val) (v2: Val) : Val =
    match v1, v2 with
    | VBool n1, VBool n2 -> VBool(n1 && n2)
    | VNat n1, VNat n2 -> VNat(n1 * n2)
    | VSet n1, VSet n2 -> VSet(Set.intersect n1 n2)
    | _ -> failwith $"cannot times: %s{format v1} vs %s{format v2}"
