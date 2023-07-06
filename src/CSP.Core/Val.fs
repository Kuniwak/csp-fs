module CSP.Core.Val

type BuiltinCtor<'C when 'C: comparison> =
    | BSome
    | BNone
    | BLeft
    | BRight

type Val<'C when 'C: comparison> =
    | ValUnit
    | ValNat of uint
    | ValBool of bool
    | ValTuple of Val<'C> * Val<'C>
    | ValSet of Set<Val<'C>>
    | ValList of Val<'C> list
    | ValMap of Map<Val<'C>, Val<'C>>
    | ValUnion of Ctor<'C> * Val<'C>
    | Error
and Ctor<'C when 'C : comparison> =
    | UDCtor of 'C
    | BCtor of BuiltinCtor<'C>