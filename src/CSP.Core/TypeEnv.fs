module CSP.Core.TypeEnv

open CSP.Core.TypeCstr
open CSP.Core.Var

type TypeEnv = Map<Var, TypeCstr>