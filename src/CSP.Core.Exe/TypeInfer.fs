module CSP.Core.Exe.TypeInfer

open System.IO
open CSP.Core.CLI.ArgParser
open CSP.Core
open CSP.Core.Expr
open CSP.Core.Sexp
open CSP.Core.Sexp.ProgramParser

type Opts = HelpNeeded | Opts of string list

let parseOpts (args: string list) : Result<Opts, string> =
    parseArgs (Map [ ("help", OTBool); ("h", OTBool) ]) args
    |> Result.map (fun opts ->
        let helpNeeded =
            opts |> List.contains (Opt("h", OVBool(true)))
            || opts |> List.contains (Opt("help", OVBool(true)))

        if helpNeeded then
            HelpNeeded
        else
            let args =
                opts
                |> List.fold
                    (fun acc opt ->
                        match opt with
                        | Arg(arg) -> arg :: acc
                        | _ -> acc)
                    []
                |> List.rev

            Opts(args))

let typeInfer (r: TextReader) (p: string) (stdout: TextWriter) : Result<unit, string> =
    let code = r.ReadToEnd() in

    parse code
    |> Result.mapError ProgramSyntaxError.format
    |> Result.bind (fun (pm, um, cm, genv) ->
        parseInit p
        |> Result.mapError ProgramSyntaxError.format
        |> Result.bind (fun p ->
            TypeChecker.typeGlobalEnv um cm genv TypeInferenceState.init
            |> Result.mapError TypeError.format
            |> Result.bind (fun (tcenv, s) ->
                TypeChecker.infer um cm tcenv pm p s
                |> Result.mapError TypeError.format
                |> Result.bind (TypeChecker.postProcess >> Result.mapError TypeError.format))))
    |> Result.map (fun (pm, p) ->
        let s = ProcMap.format typeAnnotation pm in
        stdout.WriteLine(s)
        let s = Proc.format typeAnnotation p in
        stdout.WriteLine(s))


let typeInferCLI (stdout: TextWriter) (stderr: TextWriter) (args: string list) : int =
    parseOpts args
    |> Result.bind (fun opts ->
        match opts with
        | HelpNeeded -> Error("help needed")
        | Opts(args) ->
            if List.length args < 2 then
                let s = args |> String.concat ", " in Error($"too few arguments: [%s{s}]")
            else if List.length args > 2 then
                let s = args |> String.concat ", " in Error($"too much arguments: [%s{s}]")
            else
                match args with
                | file :: [ p ] -> use r = new StreamReader(file) in typeInfer r p stdout
                | _ -> failwith "unreachable")
    |> Result.map (fun _ -> 0)
    |> Result.defaultWith (fun err ->
        stderr.WriteLine($"error: %s{err}\n\n%s{usage}")
        1)
