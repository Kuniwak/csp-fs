module CSP.Core.Var

type Var = Var of string

let format (var: Var) : string =
    match var with
    | Var var -> var

let formatOpt (varOpt: Var option) : string =
    varOpt
    |> Option.map format
    |> Option.defaultValue "_"
