module CSP.Core.ClassSize

open CSP.Core.Type
open CSP.Core.Val

let name: TypeClassName = "Size"

let derivedBy (t: Type) : bool =
    match t with
    | TList _ -> true
    | TSet _ -> true
    | TMap _ -> true
    | _ -> false

let size (v: Val) : uint =
    match v with
    | VList vs -> Checked.uint32 (List.length vs)
    | VSet s -> Checked.uint32 (Set.count s)
    | VMap m -> Checked.uint32 (Map.count m)
    | _ -> failwith $"cannot measure size: %s{format v}"
