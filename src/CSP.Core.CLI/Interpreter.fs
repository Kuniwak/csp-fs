module CSP.Core.CLI

open CSP.Core.Eval
open CSP.Core.Val
open FSharpPlus
open CSP.Core.ProcEval
open CSP.Core.Proc
open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.State
open CSP.Core.Univ
open CSP.Core.Trans

let usage =
    """? - show help message
c <i> - go to the selected state
b <i> - back to the selected state
h - show history
q - quit
"""

type InterpreterConfig =
    { TransConfig: TransConfig
      ProcEvalConfig: ProcEvalConfig }

let interpreterConfig natMax listLenMax maxUnwinding =
    let procEvalConfig =
        { EvalConfig =
            { UnivConfig =
                { NatMax = natMax
                  ListLenMax = listLenMax } }
          MaxUnwind = maxUnwinding }

    { TransConfig = { ProcEvalConfig = procEvalConfig }
      ProcEvalConfig = procEvalConfig }

let start (cfg: InterpreterConfig) (pm: ProcMap<unit>) (cm: CtorMap) (genv: Env) (pn: ProcId) (vs: Val list) =
    let format = format genv in

    match init pm genv pn vs with
    | Error(err) -> printfn $"error: %s{ProcMapError.format err}"
    | Ok(env, p) ->
        match eval cfg.ProcEvalConfig pm cm env genv p with
        | Error(err) -> printfn $"error: %s{ProcEvalError.format err}"
        | Ok(s) ->
            let mutable s = s in
            let mutable hist = [] in

            while true do
                printfn $"state: %s{State.format genv s}"
                let tsRes = trans cfg.TransConfig pm cm genv s

                match tsRes with
                | Ok(ts) ->
                    Seq.iteri (fun i (ev, s') -> printfn $"  c {i}: {Event.format ev} -> {format s'}") ts
                    printfn ""
                    printf "> "

                    let cmd = stdin.ReadLine() in

                    match String.tryHead cmd with
                    | Some '?' -> printfn $"%s{usage}"
                    | Some 'c' ->
                        let i = int (String.trimWhiteSpaces (String.drop 1 cmd)) in

                        match List.tryItem i ts with
                        | None -> printfn $"index out of range: {i}"
                        | Some(ev, s') ->
                            s <- s'
                            hist <- (s, ev) :: hist
                    | Some 'b' ->
                        let i = int (String.trimWhiteSpaces (String.drop 1 cmd)) in

                        match List.tryItem i hist with
                        | None -> printfn $"index out of range: {i}"
                        | Some(s', _) -> s <- s'
                    | Some 'h' ->
                        Seq.iteri (fun i (s', ev) -> printfn $"  b {i}: {format s'} -> {Event.format ev}") hist
                    | Some 'q' -> exit 0
                    | _ -> printfn $"%s{usage}"
                | Error(err) ->
                    printfn $"error: {ProcEvalError.format err}"
                    printfn ""
                    printf "> "

                    let cmd = stdin.ReadLine() in

                    match String.tryHead cmd with
                    | Some '?' -> printfn $"%s{usage}"
                    | Some 'b' ->
                        let i = int (String.trimWhiteSpaces (String.drop 1 cmd)) in

                        match List.tryItem i hist with
                        | None -> printfn $"index out of range: {i}"
                        | Some(s', _) -> s <- s'
                    | Some 'h' ->
                        Seq.iteri (fun i (s', ev) -> printfn $"  b {i}: {format s'} -> {Event.format ev}") hist
                    | Some 'q' -> exit 0
                    | _ -> printfn $"%s{usage}"
