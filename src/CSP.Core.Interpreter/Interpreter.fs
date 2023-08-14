module CSP.Core

open System.IO
open FSharpPlus
open CSP.Core
open CSP.Core.Eval
open CSP.Core.ProcEval
open CSP.Core.Proc
open CSP.Core.CtorMap
open CSP.Core.UnionMap
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

let interpreterConfig natMax listLenMax =
    let procEvalConfig =
        { EvalConfig =
            { UnivConfig =
                { NatMax = natMax
                  ListLenMax = listLenMax } } }

    { TransConfig = { ProcEvalConfig = procEvalConfig }
      ProcEvalConfig = procEvalConfig }

let start
    (cfg: InterpreterConfig)
    (pm: ProcMap<'a>)
    (um: UnionMap)
    (cm: CtorMap)
    (genv: Env)
    (p: Proc<'a>)
    (stdin: TextReader)
    (stdout: TextWriter)
    =
    let format = format genv in
    match eval cfg.ProcEvalConfig um cm genv p with
    | Error(err) -> 
        stdout.WriteLine $"error: {ProcEvalError.format err}"
        exit(1)
    | Ok(s) ->
        let mutable s = s
        let mutable hist = [] in

        while true do
            printfn $"state: %s{format s}"
            let tsRes = trans cfg.TransConfig pm um cm genv s

            match tsRes with
            | Ok(ts) ->
                Seq.iteri (fun i (ev, s') -> printfn $"  c {i}: {Event.format ev} -> {format s'}") ts
                printfn ""
                printf "> "

                let cmd = stdin.ReadLine() in

                match String.tryHead cmd with
                | Some '?' -> stdout.WriteLine $"%s{usage}"
                | Some 'c' ->
                    let i = int (String.trimWhiteSpaces (String.drop 1 cmd)) in

                    match List.tryItem i ts with
                    | None -> stdout.WriteLine $"index out of range: %d{i}"
                    | Some(ev, s') ->
                        s <- s'
                        hist <- (s, ev) :: hist
                | Some 'b' ->
                    let i = int (String.trimWhiteSpaces (String.drop 1 cmd)) in

                    match List.tryItem i hist with
                    | None -> stdout.WriteLine $"index out of range: %d{i}"
                    | Some(s', _) -> s <- s'
                | Some 'h' -> Seq.iteri (fun i (s', ev) -> stdout.WriteLine $"  b %d{i}: %s{format s'} -> %s{Event.format ev}") hist
                | Some 'q' -> exit 0
                | _ -> stdout.WriteLine $"%s{usage}"
            | Error(err) ->
                stdout.WriteLine $"error: %s{ProcEvalError.format err}"
                stdout.WriteLine ""
                printf "> "

                let cmd = stdin.ReadLine() in

                match String.tryHead cmd with
                | Some '?' -> stdout.WriteLine $"%s{usage}"
                | Some 'b' ->
                    let i = int (String.trimWhiteSpaces (String.drop 1 cmd)) in

                    match List.tryItem i hist with
                    | None -> stdout.WriteLine $"index out of range: %d{i}"
                    | Some(s', _) -> s <- s'
                | Some 'h' -> Seq.iteri (fun i (s', ev) -> stdout.WriteLine $"  b %d{i}: %s{format s'} -> %s{Event.format ev}") hist
                | Some 'q' -> exit 0
                | _ -> stdout.WriteLine $"%s{usage}"
