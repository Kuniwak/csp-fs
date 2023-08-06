module CSP.Core.TypeInferenceState

open CSP.Core.TypeCstr

type State =
    { UncertainVarMap: TypeCstrUncertainVar.VarMap }

let newUncertainVarId (s: State) : UncertainVarId * State =
    let id, fam = TypeCstrUncertainVar.newId s.UncertainVarMap in (id, { s with UncertainVarMap = fam })

let bindUncertainVar (id: UncertainVarId) (tc: TypeCstr) (s: State) : State =
    let fum = TypeCstrUncertainVar.bind id tc s.UncertainVarMap in { s with UncertainVarMap = fum }

let resolveUncertainVar (id: UncertainVarId) (s: State) : TypeCstr option =
    TypeCstrUncertainVar.resolve id s.UncertainVarMap

let init: State = { UncertainVarMap = TypeCstrUncertainVar.init }
