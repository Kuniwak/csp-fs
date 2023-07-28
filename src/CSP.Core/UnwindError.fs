module CSP.Core.UnwindError

open CSP.Core.EnvError
open CSP.Core.EvalError
open CSP.Core.LineNum
open CSP.Core.Proc
open CSP.Core.Var

type UnwindError =
    | At of UnwindError * string
    | Recursion of ProcId
    | EvalError of EvalError
    | EnvError of EnvError
    | ArgumentsLengthMismatch of int * Var option list
    | NoSuchProcess of ProcId * Set<ProcId>

let atLine (line: LineNum) (err: UnwindError) : UnwindError = At(err, $"line %s{line}")

let rec format (err: UnwindError) : string =
    match err with
    | At(err, hint) -> $"%s{format err}\n\tat %s{hint}"
    | Recursion(pn) -> $"recursion: %s{pn}"
    | EvalError(err) -> EvalError.format err
    | EnvError(err) -> EnvError.format err
    | ArgumentsLengthMismatch(n, varOpts) ->
        let s =
            String.concat
                " "
                (List.map
                    (fun varOpt ->
                        match varOpt with
                        | Some var -> Var.format var
                        | None -> "_")
                    varOpts)

        $"expected %d{List.length varOpts} vars as %s{s}, but got: %d{n}"
    | NoSuchProcess(pn, s) -> let s = String.concat "/" (Set.toSeq s) in $"no such process: %s{pn} in %s{s}"
