module CSP.Core.EvalError

open CSP.Core.Ctor
open CSP.Core.EnvError
open CSP.Core.Expr
open CSP.Core.LineNum
open CSP.Core.Type
open CSP.Core.UnivError
open CSP.Core.Val
open CSP.Core.Var


type EvalError =
    | At of EvalError * string
    | TypeNotDerived of Type * TypeClassName
    | ValNotBool of Val
    | ValNotUnion of Val
    | TypeMismatch of Val * Type
    | NotTuple of Val
    | TupleIndexOutOfRange of Val * uint
    | ListIndexOutOfRange of Val * uint
    | RangeLowerGreaterThanUpper of uint * uint
    | UnivError of UnivError
    | EnvError of EnvError
    | UnionValuesLenMismatch of Ctor * int * int
    | NoSuchCtor of Ctor
    | Thrown of UserDefinedErrorMessage
    | NoClauseMatched of Ctor
    | DefaultClauseArgumentLenMustBe1 of Var option list

let atLine (err: EvalError) (line: LineNum) : EvalError = At(err, $"line %s{line}")

let rec format (err: EvalError) : string =
    match err with
    | At(err, hint) -> $"%s{format err}\n\tat %s{hint}"
    | TypeNotDerived(t, cls) -> $"%s{Type.format t} not derived %s{cls}"
    | ValNotBool v -> $"value is not a bool: %s{Val.format v}"
    | ValNotUnion v -> $"value is not an union: %s{Val.format v}"
    | TypeMismatch(v, t) -> $"value {Val.format v} is not a {Type.format t}"
    | NotTuple v -> $"not tuple: %s{Val.format v}"
    | TupleIndexOutOfRange(v, n) -> $"tuple index out of range: %s{Val.format v} at %d{n}"
    | ListIndexOutOfRange(v, n) -> $"list index out of range: %s{Val.format v} at %d{n}"
    | RangeLowerGreaterThanUpper(n1, n2) -> $"lower of range is greater than upper: %d{n1} > %d{n2}"
    | UnivError err -> UnivError.format err
    | EnvError err -> EnvError.format err
    | UnionValuesLenMismatch(ctor, actual, expected) ->
        $"unexpected length of associated values: want %d{expected}, got %d{actual} at %s{Ctor.format ctor}"
    | NoSuchCtor ctor -> $"no such constructor: %s{Ctor.format ctor}"
    | Thrown msg -> $"thrown: %s{msg}"
    | NoClauseMatched ctor -> $"no clause matched: %s{Ctor.format ctor}"
    | DefaultClauseArgumentLenMustBe1 vars ->
        let s =
            String.concat
                ", "
                (List.map
                    (fun varOpt ->
                        match varOpt with
                        | Some var -> Var.format var
                        | None -> "_")
                    vars) in

        $"length of arguments for default clause must be 1, but got: [%s{s}]"
