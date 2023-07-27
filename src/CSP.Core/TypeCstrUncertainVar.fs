module CSP.Core.TypeCstrUncertainVar

open CSP.Core.TypeCstr

type VarMap =
    { Map: Map<UncertainVarId, TypeCstr>
      Next: UncertainVarId }

let bind (id: UncertainVarId) (tc: TypeCstr) (m: VarMap) =
    { m with
        Map = Map.add id tc m.Map
        Next = m.Next }

let resolve (id: UncertainVarId) (m: VarMap) : TypeCstr option = Map.tryFind id m.Map


let newId (m: VarMap) : UncertainVarId * VarMap =
    (m.Next,
     { m with
         Map = m.Map
         Next =
             match m.Next with
             | UncertainVarId n -> UncertainVarId(n + 1u) })
