module CSP.Core.TypeEnv

open CSP.Core.Type
open CSP.Core.Var

type TypeEnv = Map<Var, Type>

let from (m: (string * Type) seq) : TypeEnv =
    Map [ for var, typeCstr in m -> (Var var, typeCstr) ]

let empty: TypeEnv = Map.empty
