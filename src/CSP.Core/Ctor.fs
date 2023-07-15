module CSP.Core.Ctor

type Ctor<'Ctor when 'Ctor: comparison> =
    | Ctor of 'Ctor
    | CtorSome
    | CtorNone
    | CtorLeft
    | CtorRight
