module CSP.Core.CLI.ArgParser

open System
open FSharpPlus

type OptType =
    | OTBool
    | OTInt

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
            | Some(OTInt) ->
                match List.tryHead args with
                | Some(argValue) ->
                    let mutable i = 0u in

                    if UInt32.TryParse(argValue, &i) then
                        Ok(Opt(argName, OVNat(i)), List.drop 1 args)
                    else
                        Error($"not integer for %s{arg}: %s{argValue}")
                | None -> Error($"associated value needed for %s{arg}")
            | None -> Error($"unexpected argument: %s{arg}")
        else
            Ok(Arg(arg), List.drop 1 args)

let parseArgs (optDefs: Map<string, OptType>) (args: string list) : Result<Opt list, string> =
    let parseArg = parseArg optDefs in

    let rec parseArgs args =
        match args with
        | [] -> Ok([])
        | _ ->
            parseArg args
            |> Result.bind (fun (opt, rest) -> parseArgs rest |> Result.map (fun opts -> opt :: opts))
            |> Result.map List.rev

    parseArgs args
