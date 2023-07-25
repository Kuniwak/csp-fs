module CSP.Core.ClassEmpty

open CSP.Core.Val
open CSP.Core.Type

let name: TypeClassName = "Empty"

let derivedBy (t: Type): bool =
    match t with
    | TSet _ -> true
    | TList _ -> true
    | TMap _ -> true
    | _ -> false
    
let empty (t: Type): Val =
    match t with
    | TSet _ -> VSet(Set.empty)
    | TList _ -> VList(List.empty)
    | TMap _ -> VMap(Map.empty)
    | _ -> failwith $"cannot make empty: {format t}"
