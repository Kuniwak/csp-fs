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
    | VError

and Ctor<'Ctor when 'Ctor: comparison> =
    | Ctor of 'Ctor
    | CtorSome
    | CtorNone
    | CtorLeft
    | CtorRight
