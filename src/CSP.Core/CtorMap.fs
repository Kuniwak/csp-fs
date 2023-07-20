module CSP.Core.CtorMap

open CSP.Core.Ctor
open CSP.Core.Type

type CtorMap = Map<Ctor, UnionName * Type>
let from (m: (UnionName * Ctor * Type) seq): CtorMap = Map [for un, ctor, t in m -> (ctor, (un, t)) ]
let empty: CtorMap = Map.empty