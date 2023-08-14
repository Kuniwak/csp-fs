module CSP.Core.Exe.Validate

open System.IO
open CSP.Core
open CSP.Core.Eval
open CSP.Core.Type
open CSP.Core.UnionMap
open CSP.Core.CtorMap
open CSP.Core.ProcMap
open CSP.Core.Env
open CSP.Core.Proc
open CSP.Core.Sexp
open CSP.Core.Sexp.ProgramParser

let validate
    (cfg: EvalConfig)
    (r: TextReader)
    (p: string)
    : Result<ProcMap<unit> * UnionMap * CtorMap * Env * Proc<unit>, string> =
    let code = r.ReadToEnd() in

    parse code
    |> Result.mapError ProgramSyntaxError.format
    |> Result.bind (fun (pm, um, cm, genv) ->
        GlobalEnv.toEnv cfg um cm genv
        |> Result.mapError EvalError.format
        |> Result.bind (fun genv ->
            parseInit p
            |> Result.mapError ProgramSyntaxError.format
            |> Result.map (fun p -> (pm, um, cm, genv, p))))

let typeCheck
    (x: ProcMap<unit> * UnionMap * CtorMap * Env * Proc<unit>)
    : Result<ProcMap<Type> * UnionMap * CtorMap * Env * Proc<Type>, string> =
    let pm, um, cm, env, p = x in

    TypeChecker.typeEnv um cm env TypeInferenceState.init
    |> Result.bind (fun (tcenv, s) -> TypeChecker.infer um cm tcenv pm p s)
    |> Result.bind TypeChecker.postProcess
    |> Result.mapError TypeError.format
    |> Result.map (fun (pm, p) -> (pm, um, cm, env, p))
