module CSP.Core.TypeCstrForAllVar

open CSP.Core.TypeCstr

type VarMap =
    { Map: Map<ForAllVarId, TypeCstr>
      Next: ForAllVarId }

let fix (id: ForAllVarId) (tc: TypeCstr) (m: VarMap) =
    { m with
        Map = Map.add id tc m.Map
        Next = m.Next }

let number (m: VarMap) : ForAllVarId * VarMap =
    (m.Next,
     { m with
         Map = m.Map
         Next =
             match m.Next with
             | ForAllVarId n -> ForAllVarId(n + 1u) })
