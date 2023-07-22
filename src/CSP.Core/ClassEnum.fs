module CSP.Core.ClassEnum

open CSP.Core.Type

let name: TypeClassName = "Enum"

let rec derivedBy (t: Type) : bool =
    match t with
    | TSet _ -> true
    | TList _ -> true
    | TMap _ -> true
    | _ -> false
