module CSP.Core.Val


type Val<'Ctor when 'Ctor: comparison> =
    | VUnit
    | VNat of uint
    | VBool of bool
    | VTuple of Val<'Ctor> * Val<'Ctor>
    | VSet of Set<Val<'Ctor>>
    | VList of Val<'Ctor> list
    | VMap of Map<Val<'Ctor>, Val<'Ctor>>
    | VUnion of Ctor<'Ctor> * Val<'Ctor>
    | VAny
    | VError

and Ctor<'Ctor when 'Ctor: comparison> =
    | Ctor of 'Ctor
    | CtorSome
    | CtorNone
    | CtorLeft
    | CtorRight

let rec format (v: Val<'Ctor>) : string =
    match v with
    | VUnit -> "()"
    | VNat n -> $"{n}"
    | VBool b -> $"{b}"
    | VTuple(l, r) -> $"({format l}, {format r})"
    | VSet(s) -> let s' = String.concat ", " (List.map format (Set.toList s)) in $"{{{s'}}}"
    | VList(vs) -> let s' = String.concat ", " (List.map format vs) in $"[{s'}]"
    | VMap(m) -> let s' = String.concat ", " (List.map (fun (k, v) -> $"{format k}: {format v}") (Map.toList m)) in $"{{{s'}}}"
    | VUnion(c, v) -> $"({c} {format v})"
    | VAny -> "*"
    | VError -> "ERROR"
