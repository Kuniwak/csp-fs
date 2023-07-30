module CSP.Core.ProcEvalError

open CSP.Core.Ctor
open CSP.Core.EnvError
open CSP.Core.EvalError
open CSP.Core.Proc
open CSP.Core.ProcMapError
open CSP.Core.Val
open CSP.Core.Var


type ProcEvalError =
    | ExprError of EvalError
    | ProcMapError of ProcMapError
    | ValNotSet of Val
    | ValNotBool of Val
    | ValNotUnion of Val
    | AssociatedValuesLengthMismatch of Ctor * Var option list * Val list
    | NoClauseMatched of Ctor * Val list
    | DefaultClauseArgumentLenMustBe1 of Var option list
    | Recursion of ProcId * Val list

let format (err: ProcEvalError): string =
    match err with
    | ExprError(err) -> EvalError.format err
    | ProcMapError(err) -> ProcMapError.format err
    | ValNotSet(v) -> $"expected a set, but got: %s{Val.format v}"
    | ValNotBool(v) -> $"expected a bool, but got: %s{Val.format v}"
    | ValNotUnion(v) -> $"expected an union, but got: %s{Val.format v}"
    | AssociatedValuesLengthMismatch(ctor, varOpts, vs) ->
        let s1 = String.concat " " (Seq.map (fun varOpt -> match varOpt with Some(var) -> format var | None -> "_" ) varOpts) in
        let s2 = String.concat " " (Seq.map Val.format vs) in
        $"length of associated values for %s{Ctor.format ctor} mismatch: (%s{s1}) vs (${s2})"
    | NoClauseMatched(ctor, vs) ->
        let s = String.concat " " (Seq.map Val.format vs)
        $"no clause matched: %s{Ctor.format ctor} %s{s}"
    | DefaultClauseArgumentLenMustBe1(varOpts) ->
        let s = String.concat " " (Seq.map (fun varOpt -> match varOpt with Some(var) -> format var | None -> "_" ) varOpts) in
        $"no clause matched: (x) vs (%s{s})"
    | Recursion(pn, vs) ->
        let s = String.concat " " (Seq.map Val.format vs) in
        $"recursion: %s{pn} %s{s}"
        
    
