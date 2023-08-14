module CSP.Core.Sexp.ExprSyntaxError

open CSP.Core.Sexp.Sexp
open CSP.Core.Sexp.MapperError
open CSP.Core.Sexp.TypeSyntaxError

type ExprSyntaxError =
    | At of ExprSyntaxError * string
    | TypeSyntaxError of TypeSyntaxError
    | MapperError of MapperError
    | TooFewTypeArguments of Sexp list
    | UnexpectedIdentifier of string
    | ExpectedCtor
    | ExpectedExpr

let rec format (err: ExprSyntaxError) : string =
    match err with
    | At(err, hint) -> $"%s{format err}\n\tat %s{hint}"
    | TypeSyntaxError(err) -> TypeSyntaxError.format err
    | TooFewTypeArguments(ts) ->
        let s = ts |> Seq.map Sexp.format |> String.concat " " in $"too few type arguments: %s{s}"
    | MapperError(err) -> MapperError.format err
    | UnexpectedIdentifier(s) -> $"identifier must start with ASCII letter: %s{s}"
    | ExpectedCtor -> "expected a constructor"
    | ExpectedExpr -> "expected an expression"
