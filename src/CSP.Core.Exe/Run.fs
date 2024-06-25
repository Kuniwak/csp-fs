module CSP.Core.Exe.Run

open System.IO
open FSharpPlus
open CSP.Core
open CSP.Core.Eval
open CSP.Core.ProcEval
open CSP.Core.Exe.Validate
open CSP.Core.Exe.Usage
open CSP.Core.CLI.ArgParser

type RunConfig =
    { InterpreterConfig: InterpreterConfig
      EvalConfig: EvalConfig }

type Opts =
    | NeedHelp
    | Opts of uint * uint * string list

let parseOpts (args: string list) : Result<Opts, string> =
    parseArgs (Map [ ("nat-max", OTNat); ("list-max", OTNat); ("help", OTBool) ]) args
    |> Result.map (fun opts ->
        let needHelp = opts |> List.contains (Opt("help", OVBool(true))) in

        if needHelp then
            NeedHelp
        else
            let natMax =
                opts
                |> List.fold
                    (fun acc opt ->
                        match opt with
                        | Opt("nat-max", OVNat(n)) -> Some(n)
                        | _ -> acc)
                    None
                |> Option.defaultValue 3u in

            let listMax =
                opts
                |> List.fold
                    (fun acc opt ->
                        match opt with
                        | Opt("list-max", OVNat(n)) -> Some(n)
                        | _ -> acc)
                    None
                |> Option.defaultValue 3u in

            let args =
                opts
                |> List.fold
                    (fun acc opt ->
                        match opt with
                        | Arg(arg) -> arg :: acc
                        | _ -> acc)
                    []
                |> List.rev

            Opts(natMax, listMax, args))

let run (r: TextReader) (cfg: RunConfig) (p: string) (stdin: TextReader) (stdout: TextWriter) : Result<unit, string> =
    validate cfg.EvalConfig r p
    |> Result.map (fun (pm, um, cm, genv, p) -> start cfg.InterpreterConfig pm um cm genv p stdin stdout)

let runCLI (stdin: TextReader) (stdout: TextWriter) (stderr: TextWriter) (args: string list) : int =
    parseOpts args
    |> Result.bind (fun opts ->
        match opts with
        | NeedHelp -> Error($"help needed\n\n%s{usage}")
        | Opts(natMax, listMax, args) ->
            let evalCfg =
                { UnivConfig =
                    { NatMax = natMax
                      ListLenMax = listMax }
                  PlusConfig =
                      { NatMax = natMax
                        ListLenMax = listMax }}

            let procEvalCfg = { EvalConfig = evalCfg }

            let cfg =
                { InterpreterConfig =
                    { TransConfig = { ProcEvalConfig = procEvalCfg }
                      ProcEvalConfig = procEvalCfg }
                  EvalConfig = evalCfg }

            if List.length args < 2 then
                let s = args |> String.concat ", " in Error($"too few arguments: [%s{s}]\n\n%s{usage}")
            else if List.length args > 2 then
                let s = args |> String.concat ", " in Error($"too much arguments: [%s{s}]\n\n%s{usage}")
            else
                match args with
                | file :: [ p ] -> use r = new StreamReader(file) in run r cfg p stdin stdout
                | _ -> failwith "unreachable")
    |> Result.map (fun _ -> 0)
    |> Result.defaultWith (fun err ->
        stderr.WriteLine($"error: %s{err}")
        1)
