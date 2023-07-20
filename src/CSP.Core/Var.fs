module CSP.Core.Var

type Var = Var of string

let format (var: Var) : string =
    match var with
    | Var var -> var
