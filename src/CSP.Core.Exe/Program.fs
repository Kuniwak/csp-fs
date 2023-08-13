open CSP.Core.Exe.Usage
open CSP.Core.Exe.Version
open CSP.Core.Exe.Run
open CSP.Core.Exe.Dot

module Program =
    [<EntryPoint>]
    let main args =
        use stdin = stdin in
        use stdout = stdout in
        use stderr = stdout in
        let args = List.ofArray args in

        if List.length args < 1 then
            printfn $"%s{usage}"
            1
        else
            match args with
            | "version" :: _ ->
                printfn $"%s{version}"
                0
            | "run" :: args -> runCLI stdin stdout stderr args
            | "dot" :: args -> dotCLI stdout stderr args
            | unknown :: _ ->
                stderr.WriteLine($"error: no such command: %s{unknown}")
                stderr.WriteLine(usage)
                1
            | _ -> failwith "unreachable"
