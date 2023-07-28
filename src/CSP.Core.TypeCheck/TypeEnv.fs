module CSP.Core.TypeEnv

open CSP.Core.Type
open CSP.Core.Var
open CSP.Core.TypeEnvError

type TypeEnv = TypeEnv of Map<Var, Type>

let bind1 (var: Var) (tc: Type) (tcenv: TypeEnv) : Result<TypeEnv, TypeEnvError> =
    match tcenv with
    | TypeEnv m ->
        if Map.containsKey var m then
            Error(Shadow(var, Set.ofSeq (Map.keys m)))
        else
            Ok(TypeEnv(Map.add var tc m))

let from (m: (string * Type) seq) : TypeEnv =
    TypeEnv(Map [ for var, typeCstr in m -> (Var var, typeCstr) ])

let empty: TypeEnv = TypeEnv Map.empty

let fold folder s tenv =
    match tenv with
    | TypeEnv m -> Map.fold folder s m
