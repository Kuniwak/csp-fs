module CSP.Core.ProcMapError

open CSP.Core.Proc

type ProcMapError = DuplicatedProcId of ProcId

let format (err: ProcMapError) : string =
    match err with
    | DuplicatedProcId(pn) -> $"duplicated process found: %s{pn}"
