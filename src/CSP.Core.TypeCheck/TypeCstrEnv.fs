module CSP.Core.TypeCstrEnv

open CSP.Core.TypeCstr
open CSP.Core.TypeEnvError
open CSP.Core.Var

type TypeCstrEnv = TypeCstrEnv of Map<Var, TypeCstr>

let empty: TypeCstrEnv = TypeCstrEnv Map.empty

let from (xs: (string * TypeCstr) seq) =
    TypeCstrEnv(Map [ for var, tc in xs -> (Var var, tc) ])

let bind1 (var: Var) (tc: TypeCstr) (tcenv: TypeCstrEnv) : Result<TypeCstrEnv, TypeEnvError> =
    match tcenv with
    | TypeCstrEnv m ->
        if Map.containsKey var m then
            Error(Shadow(var, Set.ofSeq (Map.keys m)))
        else
            Ok(TypeCstrEnv(Map.add var tc m))

let bindAll (xs: (Var option * TypeCstr) seq) (tcenv: TypeCstrEnv) : Result<TypeCstrEnv, TypeEnvError> =
    Seq.fold
        (fun envRes (varOpt, tc) ->
            match varOpt with
            | Some var -> Result.bind (bind1 var tc) envRes
            | None -> envRes)
        (Ok(tcenv))
        xs

let tryFind (var: Var) (tcenv: TypeCstrEnv) : Result<TypeCstr, TypeEnvError> =
    match tcenv with
    | TypeCstrEnv tcenv ->
        match Map.tryFind var tcenv with
        | Some(tc) -> Ok(tc)
        | None -> Error(UnboundVariable(var))
