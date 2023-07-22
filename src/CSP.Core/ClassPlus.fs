module CSP.Core.ClassPlus

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Plus"

let derivedBy (t: Type) : bool =
    match t with
    | TBool -> true
    | TNat -> true
    | TSet _ -> true
    | TList _ -> true
    | _ -> false

let plus (v1: Val) (v2: Val) : Val =
    match v1, v2 with
    | VBool n1, VBool n2 -> VBool(n1 || n2)
    | VNat n1, VNat n2 -> VNat(n1 + n2)
    | VSet n1, VSet n2 -> VSet(Set.union n1 n2)
    | VList n1, VList n2 -> VList(List.append n1 n2)
    | _ -> failwith $"cannot plus: %s{format v1} vs %s{format v2}"
