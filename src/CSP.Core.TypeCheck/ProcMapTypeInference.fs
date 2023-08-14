module CSP.Core.ProcMapTypeInference

open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.TypeError
open CSP.Core.CtorMap
open CSP.Core.UnionMap
open CSP.Core.TypeCstrEnv
open CSP.Core.ProcMap
open CSP.Core.ProcTypeInference
open CSP.Core.TypeInferenceState
open CSP.Core.TypeGeneralization
open CSP.Core.Util

let infer
    (um: UnionMap)
    (cm: CtorMap)
    (tcenv: TypeCstrEnv)
    (pm: ProcMap<unit>)
    (s: State)
    : Result<ProcMap<TypeCstr> * State, TypeError> =
    pm
    |> fold
        (fun sRes pn (varDecls, p) ->
            sRes
            |> Result.bind (fun (m, s) ->
                let xs, s =
                    List.foldBack
                        (fun (var, t) (xs, s) -> let tc, s, _ = generalize t s Map.empty in ((var, tc) :: xs, s))
                        varDecls
                        ([], s)
                        
                let tcenv = bindAll xs tcenv in

                match infer pm um cm tcenv p s with
                | Error(err) -> Error(At(err, $"process `{pn}`"))
                | Ok(p, s) -> Ok(Map.add pn (varDecls, p) m, s)))
        (Ok(Map.empty, s))
    |> Result.map (fun (m, s) -> (ProcMap m, s))

let resolve (s: State) (pm: ProcMap<TypeCstr>) : Result<ProcMap<TypeCstr>, TypeError> =
    let pm = map (ExprTypeInference.resolve s >> Result.map Expr.get) pm in

    match error pm with
    | Some(terr) -> Error(terr)
    | None -> Ok(map (Expr.get >> ResultEx.getValue TypeError.format) pm)

let instantiate (pm: ProcMap<TypeCstr>) : ProcMap<Type> =
    map (ExprTypeInference.instantiate >> Expr.get) pm
    
let postProcess (res: Result<ProcMap<TypeCstr> * State, TypeError>) : Result<ProcMap<Type> * State, TypeError> =
    res
    |> Result.bind (fun (pm, s) -> resolve s pm |> Result.map (fun p -> (p, s)))
    |> Result.map (fun (p, s) -> (instantiate p, s))
