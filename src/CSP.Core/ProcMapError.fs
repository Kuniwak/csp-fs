module CSP.Core.ProcMapError

open CSP.Core.Proc

type ProcMapError = DuplicatedProcId of Set<ProcId>

let format (err: ProcMapError) : string =
    match err with
    | DuplicatedProcId(pns) ->
        let s = pns |> Set.toSeq |> String.concat ", " in
        $"duplicated process found: [%s{s}]"
