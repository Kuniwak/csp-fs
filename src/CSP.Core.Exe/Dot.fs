module CSP.Core.Exe.Dot

open System.IO
open CSP.Core
open CSP.Core.Eval
open CSP.Core.ProcEval
open CSP.Core.Univ
open CSP.Core.Search
open CSP.Core.Visualization.DotLang
open CSP.Core.CLI.ArgParser
open CSP.Core.Exe.Validate
open CSP.Core.Exe.Usage

type DotConfig =
    { GraphConfig: GraphConfig
      EvalConfig: EvalConfig }

type Opts =
    | HelpNeeded
    | Opts of uint * uint * uint * string list

let parseOpts (args: string list) : Result<Opts, string> =
    parseArgs (Map [ ("nat-max", OTNat); ("list-max", OTNat); ("help", OTBool); ("h", OTBool) ]) args
    |> Result.map (fun opts ->
        let helpNeeded =
            opts |> List.contains (Opt("h", OVBool(true)))
            || opts |> List.contains (Opt("help", OVBool(true)))

        if helpNeeded then
            HelpNeeded
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

            let nodeMax =
                opts
                |> List.fold
                    (fun acc opt ->
                        match opt with
                        | Opt("node-max", OVNat(n)) -> Some(n)
                        | _ -> acc)
                    None
                |> Option.defaultValue 10000u in

            let args =
                opts
                |> List.fold
                    (fun acc opt ->
                        match opt with
                        | Arg(arg) -> arg :: acc
                        | _ -> acc)
                    []
                |> List.rev

            Opts(natMax, listMax, nodeMax, args))

let dot (r: TextReader) (cfg: DotConfig) (p: string) (stdout: TextWriter) : Result<unit, string> =
    validate cfg.EvalConfig r p
    |> Result.bind (fun (pm, um, cm, genv, p) -> dot cfg.GraphConfig pm um cm genv p |> Result.mapError ProcEvalError.format)
    |> Result.map (fun dot -> stdout.WriteLine(dot))

let dotCLI (stdout: TextWriter) (stderr: TextWriter) (args: string list) : int =
    parseOpts args
    |> Result.bind (fun opts ->
        match opts with
        | HelpNeeded -> Error("help needed")
        | Opts(natMax, listMax, nodeMax, args) ->
            let univCfg: UnivConfig =
                { NatMax = natMax
                  ListLenMax = listMax }

            let evalCfg: EvalConfig = { UnivConfig = univCfg }
            let procEvalCfg: ProcEvalConfig = { EvalConfig = evalCfg }

            let cfg: DotConfig =
                { GraphConfig =
                    { TransConfig = { ProcEvalConfig = procEvalCfg }
                      ProcEvalConfig = procEvalCfg
                      SearchConfig = { NodeMax = nodeMax }
                      NamedConfig =
                        { UnivConfig = univCfg
                          ProcEvalConfig = procEvalCfg } }
                  EvalConfig = evalCfg }

            if List.length args < 2 then
                let s = args |> String.concat ", " in Error($"too few arguments: [%s{s}]")
            else if List.length args > 2 then
                let s = args |> String.concat ", " in Error($"too much arguments: [%s{s}]")
            else
                match args with
                | file :: [ p ] -> use r = new StreamReader(file) in dot r cfg p stdout
                | _ -> failwith "unreachable")
    |> Result.map (fun _ -> 0)
    |> Result.defaultWith (fun err ->
        stderr.WriteLine($"error: %s{err}\n\n%s{usage}")
        1)
