module CSP.Core.CLI

open FSharpPlus
open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.ProcMap
open CSP.Core.Val
open CSP.Core.State
open CSP.Core.Trans

let usage =
    """? - show help message
c <i> - go to the selected state
b <i> - back to the selected state
h - show history
q - quit
"""

let start (pm: ProcMap<'P, 'Var, 'Ctor>) (cm: CtorMap<'Ctor>) (genv: Env<'Var, 'Ctor>) (n: 'P) (vOpt: Val<'Ctor> option) =
    let mutable s = init pm genv n vOpt in
    let mutable hist = []

    while true do
        printfn $"state: %s{format pm cm s}"
        let ts = trans pm cm genv s

        Seq.iteri (fun i (ev, s') -> printfn $"  c {i}: {Event.format ev} -> {format pm cm s'}") ts
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
            Seq.iteri
                (fun i (s', ev) -> printfn $"  b {i}: {format pm cm s'} -> {Event.format ev}")
                hist
        | Some 'q' -> exit 0
        | _ -> printfn $"%s{usage}"
