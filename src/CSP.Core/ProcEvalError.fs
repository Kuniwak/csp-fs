module CSP.Core.ProcEvalError

open CSP.Core.Ctor
open CSP.Core.EvalError
open CSP.Core.Proc
open CSP.Core.ProcMapError
open CSP.Core.UnivError
open CSP.Core.Util
open CSP.Core.Val
open CSP.Core.Var


type ProcEvalError =
    | ExprError of EvalError
    | ProcMapError of ProcMapError
    | UnivError of UnivError
    | ValNotSet of Val
    | ValNotBool of Val
    | ValNotUnion of Val
    | AssociatedValuesLengthMismatch of Ctor * Var option list * Val list
    | NoClauseMatched of Ctor * Val list
    | DefaultClauseArgumentLenMustBe1 of Var option list
    | Recursion of ProcId * Val list
    | NoSuchProcess of ProcId
    | ArgumentsLengthMismatch of ProcId * Var list * Val list

let format (err: ProcEvalError) : string =
    match err with
    | ExprError(err) -> EvalError.format err
    | ProcMapError(err) -> ProcMapError.format err
    | UnivError(err) -> UnivError.format err
    | ValNotSet(v) -> $"expected a set, but got: %s{Val.format v}"
    | ValNotBool(v) -> $"expected a bool, but got: %s{Val.format v}"
    | ValNotUnion(v) -> $"expected an union, but got: %s{Val.format v}"
    | AssociatedValuesLengthMismatch(ctor, varOpts, vs) ->
        let s1 = varOpts |> Seq.map formatOpt |> String.concat " " in
        let s2 = vs |> Seq.map Val.format |> String.concat " " in
        $"length of associated values for %s{Ctor.format ctor} mismatch: (%s{s1}) vs (${s2})"
    | NoClauseMatched(ctor, vs) ->
        let s = String.concat " " (Seq.map Val.format vs) in $"no clause matched: %s{Ctor.format ctor} %s{s}"
    | DefaultClauseArgumentLenMustBe1(varOpts) ->
        let s = varOpts |> Seq.map formatOpt |> String.concat " " in $"no clause matched: (x) vs (%s{s})"
    | Recursion(pn, vs) -> let s = String.concat " " (Seq.map Val.format vs) in $"recursion: %s{pn} %s{s}"
    | ArgumentsLengthMismatch(pn, varOpts, vs) ->
        let s1 = varOpts |> Seq.map format |> String.concat " " |> StringEx.wrapBy "(" ")" in
        let s2 = vs |> Seq.map Val.format |> String.concat " " |> StringEx.wrapBy "(" ")" in
        $"arguments length mismatch: %s{pn} %s{s1} vs %s{s2}"
    | NoSuchProcess(pn) -> $"no such process: %s{pn}"
