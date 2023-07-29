module CSP.Core.CLI

open FSharpPlus
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.CtorMap
open CSP.Core.Env
open CSP.Core.ProcMap
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
      TransConfig: TransConfig
      InitConfig: InitConfig }

let interpreterConfig natMax listLenMax =
    let evalConfig = evalConfig (univConfig natMax listLenMax) in

    { UnwindConfig = unwindConfig evalConfig
      TransConfig = transConfig evalConfig
      InitConfig = initConfig evalConfig }

let start (cfg: InterpreterConfig) (pm: ProcMap<unit>) (cm: CtorMap) (genv: Env) (n: ProcId) (exprs: Expr<unit> list) =
    let format = format genv in
    let trans = trans cfg.TransConfig pm cm genv in

    let mutable s =
        match init cfg.InitConfig cm pm genv n exprs with
        | Ok(s) -> s
        | Error(err) -> ErrorState(TransError.format err)

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
