module CSP.Core.TypeCstrUncertainVar

open CSP.Core.TypeCstr

type VarMap =
    { Map: Map<UncertainVarId, TypeCstr>
      Next: UncertainVarId }

let fix (id: UncertainVarId) (tc: TypeCstr) (m: VarMap) =
    { m with
        Map = Map.add id tc m.Map
        Next = m.Next }

let number (m: VarMap) : UncertainVarId * VarMap =
    (m.Next,
     { m with
         Map = m.Map
         Next =
             match m.Next with
             | UncertainVarId n -> UncertainVarId(n + 1u) })
