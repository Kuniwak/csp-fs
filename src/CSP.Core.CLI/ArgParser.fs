module CSP.Core.CLI.ArgParser

open System
open FSharpPlus

type OptType =
    | OTBool
    | OTNat

type OptVal =
    | OVBool of bool
    | OVNat of uint

type Opt =
    | Opt of string * OptVal
    | Arg of string

let parseArg (optDefs: Map<string, OptType>) (args: string list) : Result<Opt * string list, string> =
    match args with
    | [] -> Error("expected at least one arguments, but got empty")
    | arg :: args ->
        if String.startsWith "-" arg then
            let argName = String.trimStart [ '-' ] arg

            match Map.tryFind argName optDefs with
            | Some(OTBool) -> Ok(Opt(argName, OVBool(true)), args)
            | Some(OTNat) ->
                match args with
                | [] -> Error($"associated value needed for %s{arg}")
                | argValue :: args ->
                    let mutable i = 0u in

                    if UInt32.TryParse(argValue, &i) then
                        Ok(Opt(argName, OVNat(i)), args)
                    else
                        Error($"not integer for %s{arg}: %s{argValue}")
            | None -> Error($"unexpected argument: %s{arg}")
        else
            Ok(Arg(arg), args)

let parseArgs (optDefs: Map<string, OptType>) (args: string list) : Result<Opt list, string> =
    let parseArg = parseArg optDefs in

    let rec parseArgs args =
        match args with
        | [] -> Ok([])
        | _ ->
            parseArg args
            |> Result.bind (fun (opt, rest) -> parseArgs rest |> Result.map (fun opts -> opt :: opts))

    parseArgs args
