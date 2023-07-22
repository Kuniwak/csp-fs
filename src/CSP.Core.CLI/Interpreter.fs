module CSP.Core.CLI

open FSharpPlus
open CSP.Core.Proc
open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.Val
open CSP.Core.State
open CSP.Core.Univ
open CSP.Core.Eval
open CSP.Core.Trans

let usage =
    """? - show help message
c <i> - go to the selected state
b <i> - back to the selected state
h - show history
q - quit
"""

let natMax = 5u
let listLenMax = 3u

type InterpreterConfig =
    { UnwindConfig: UnwindConfig
      TransConfig: TransConfig }

let interpreterConfig natMax listLenMax =
    transConfig (evalConfig (univConfig natMax listLenMax))

let cfg: TransConfig = interpreterConfig natMax listLenMax

let start (pm: ProcMap) (cm: CtorMap) (genv: Env) (n: ProcId) (vOpt: Val option) =
    let format = format cfg.UnwindConfig pm cm genv in
    let trans = trans cfg pm cm genv in
    let mutable s = init pm genv n vOpt in
    let mutable hist = [] in

    while true do
        printfn $"state: %s{format s}"
        let ts = trans s

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
