module CSP.Core.ProcMapError

open CSP.Core.Proc
open CSP.Core.Util
open CSP.Core.Val
open CSP.Core.Var

type ProcMapError =
    | DuplicatedProcId of ProcId
    | NoSuchProcess of ProcId
    | ArgumentsLengthMismatch of ProcId * Var option list * Val list

let format (err: ProcMapError) : string =
    match err with
    | DuplicatedProcId(pn) -> $"duplicated process found: %s{pn}"
    | NoSuchProcess(pn) -> $"no such process: %s{pn}"
    | ArgumentsLengthMismatch(pn, varOpts, vs) ->
        let s1 =
            varOpts |> Seq.map formatOpt |> String.concat ", " |> StringEx.wrapBy "(" ")"

        let s2 = vs |> Seq.map Val.format |> String.concat ", " |> StringEx.wrapBy "(" ")"
        $"process arguments length mismatch: %s{pn} %s{s1} vs %s{s2}"
