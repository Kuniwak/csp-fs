module CSP.Core.CtorMap

open CSP.Core.Ctor
open CSP.Core.Type

type CtorMap = Map<Ctor, UnionName * Type>
