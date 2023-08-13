module CSP.Core.GlobalEnvError

open CSP.Core.Var

type GlobalEnvError =
    | At of GlobalEnvError * string
    | DuplicatedVar of Var
    
let rec format (err: GlobalEnvError): string =
    match err with
    | At(err, hint) -> $"%s{format err}\n\tat %s{hint}"
    | DuplicatedVar(var) -> $"duplicated variable: %s{Var.format var}"