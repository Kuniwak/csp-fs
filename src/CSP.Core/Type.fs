module CSP.Core.Type

type Ctor<'Ctor when 'Ctor: comparison> =
    | Ctor of 'Ctor
    | CtorSome
    | CtorNone
    | CtorLeft
    | CtorRight
    
type Type<'Ctor when 'Ctor: comparison> =
    | TUnit
    | TNat
    | TBool
    | TTuple of Type<'Ctor> * Type<'Ctor>
    | TSet of Type<'Ctor>
    | TList of Type<'Ctor>
    | TMap of Type<'Ctor> * Type<'Ctor>
    | TUnion of string * Map<Ctor<'Ctor>, Type<'Ctor>>
    | TEvent of Type<'Ctor>
    | TError

let tOption (t: Type<'Ctor>) = TUnion("option", Map [(CtorSome, t); (CtorNone, TUnit)])
let tEither (tl: Type<'Ctor>) (tr: Type<'Ctor>) = TUnion("either", Map [(CtorLeft, tl); (CtorRight, tr)])
let tTriple (t1: Type<'Ctor>) (t2: Type<'Ctor>) (t3: Type<'Ctor>) = TTuple(t1, TTuple(t2, t3))

let rec format (t: Type<'Ctor>): string =
    match t with
    | TUnit -> "unit"
    | TNat -> "nat"
    | TBool -> "bool"
    | TTuple(lt, rt) -> $"({format lt} * {format rt})"
    | TSet t -> $"({format t} set)"
    | TList t -> $"({format t} list)"
    | TMap(tk, tv) -> $"(({format tk}, {format tv}) map)"
    | TUnion(n, _) -> n
    | TEvent t -> $"({format t} event)"
    | TError -> "error"