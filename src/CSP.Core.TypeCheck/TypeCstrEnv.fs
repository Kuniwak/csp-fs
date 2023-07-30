module CSP.Core.TypeCstrEnv

open CSP.Core.TypeCstr
open CSP.Core.TypeEnvError
open CSP.Core.Var

type TypeCstrEnv = TypeCstrEnv of Map<Var, TypeCstr>

let empty: TypeCstrEnv = TypeCstrEnv Map.empty

let from (xs: (string * TypeCstr) seq) =
    TypeCstrEnv(Map [ for var, tc in xs -> (Var var, tc) ])

let bind1 (var: Var) (tc: TypeCstr) (tcenv: TypeCstrEnv) : TypeCstrEnv =
    match tcenv with
    | TypeCstrEnv m -> TypeCstrEnv(Map.add var tc m)

let bindAll (xs: (Var * TypeCstr) seq) (tcenv: TypeCstrEnv) : TypeCstrEnv =
    Seq.fold (fun env (var, tc) -> bind1 var tc env) tcenv xs

let bind1Opt (varOpt: Var option) (tc: TypeCstr) (tcenv: TypeCstrEnv) : TypeCstrEnv =
    match varOpt with
    | Some(var) -> bind1 var tc tcenv
    | None -> tcenv

let bindAllOpts (xs: (Var option * TypeCstr) seq) (tcenv: TypeCstrEnv) : TypeCstrEnv =
    Seq.fold (fun env (varOpt, tc) -> bind1Opt varOpt tc env) tcenv xs

let tryFind (var: Var) (tcenv: TypeCstrEnv) : Result<TypeCstr, TypeEnvError> =
    match tcenv with
    | TypeCstrEnv tcenv ->
        match Map.tryFind var tcenv with
        | Some(tc) -> Ok(tc)
        | None -> Error(UnboundVariable(var))
