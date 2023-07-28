module CSP.Core.TransError

open CSP.Core.EnvError
open CSP.Core.EvalError
open CSP.Core.LineNum
open CSP.Core.UnwindError
open CSP.Core.Val

type TransError =
    | At of TransError * string
    | NotSet of Val
    | EvalError of EvalError
    | EnvError of EnvError
    | UnwindError of UnwindError

let atLine (line: LineNum) (err: TransError) : TransError = At(err, $"line %s{line}")

let rec format (err: TransError) : string =
    match err with
    | At(err, hint) -> $"%s{format err}\n\tat %s{hint}"
    | NotSet(v) -> $"value is not a set: %s{Val.format v}"
    | EnvError(err) -> EnvError.format err
    | EvalError(err) -> EvalError.format err
    | UnwindError(err) -> UnwindError.format err
