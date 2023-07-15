module CSP.Core.CtorMap

open CSP.Core.Ctor
open CSP.Core.Type

type CtorMap<'Ctor when 'Ctor: comparison> = Map<Ctor<'Ctor>, string * Type>
