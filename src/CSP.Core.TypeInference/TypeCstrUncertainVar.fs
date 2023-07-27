module CSP.Core.TypeCstrUncertainVar

open CSP.Core.TypeCstr

type VarMap =
    { Map: Map<UncertainVarId, TypeCstr>
      Next: UncertainVarId }

let init: VarMap =
    { Map = Map.empty
      Next = UncertainVarId 0u }

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

let format (m: VarMap) : string =
    String.concat "\n" (List.map (fun (u, tc) -> $"%s{format (TCUncertain u)} -> %s{format tc}") (Map.toList m.Map))
