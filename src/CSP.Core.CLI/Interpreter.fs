module CSP.Core.CLI

open CSP.Core.Eval
open CSP.Core.Val
open FSharpPlus
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

let start (cfg: InterpreterConfig) (pm: ProcMap<unit>) (um: UnionMap) (cm: CtorMap) (genv: Env) (pn: ProcId) (vs: Val list) =
    let format = format genv in

    let mutable s = Unwind(pn, vs) in
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
            | Some 'h' -> Seq.iteri (fun i (s', ev) -> printfn $"  b {i}: {format s'} -> {Event.format ev}") hist
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
            | Some 'h' -> Seq.iteri (fun i (s', ev) -> printfn $"  b {i}: {format s'} -> {Event.format ev}") hist
            | Some 'q' -> exit 0
            | _ -> printfn $"%s{usage}"
