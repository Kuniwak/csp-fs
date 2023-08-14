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
open CSP.Core.TypeChecker
open CSP.Core.Sexp
open CSP.Core.Sexp.ProgramParser

let validate
    (cfg: EvalConfig)
    (r: StreamReader)
    (p: string)
    : Result<ProcMap<Type> * UnionMap * CtorMap * Env * Proc<Type>, string> =
    let code = r.ReadToEnd() in

    parse code
    |> Result.mapError ProgramSyntaxError.format
    |> Result.bind (fun (pm, um, cm, genv) ->
        GlobalEnv.toEnv cfg um cm genv
        |> Result.mapError EvalError.format
        |> Result.bind (fun genv ->
            parseInit p
            |> Result.mapError ProgramSyntaxError.format
            |> Result.bind (fun p ->
                typeCheck um cm genv pm p
                |> Result.mapError TypeError.format
                |> Result.map (fun  (pm, p)  -> (pm, um, cm, genv, p)))))
