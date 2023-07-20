module CSP.Core.TypeEnv

open CSP.Core.TypeCstr
open CSP.Core.Var

type TypeEnv = Map<Var, TypeCstr>

let from (m: (string * TypeCstr) seq) : TypeEnv =
    Map [ for var, typeCstr in m -> (Var var, typeCstr) ]

let empty: TypeEnv = Map.empty
