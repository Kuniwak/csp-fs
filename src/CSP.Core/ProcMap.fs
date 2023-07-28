module CSP.Core.ProcMap

open CSP.Core.Proc
open CSP.Core.Var

type ProcMap<'a> = ProcMap of Map<ProcId, Var option * Proc<'a>>

let from (pm: ((ProcId * string) * Proc<'a>) seq) : ProcMap<'a> =
    ProcMap(Map [ for (pn, var), p in pm -> (pn, ((if var = "_" then None else Some(Var var)), p)) ])

let fold folder s pm =
    match pm with
    | ProcMap m -> Map.fold folder s m

let tryFind pn pm =
    match pm with
    | ProcMap m -> Map.tryFind pn m

let formatNames (pm: ProcMap<'a>): string =
    match pm with
    | ProcMap m ->
        String.concat
            "\n"
            (Seq.map
                (fun (n, (varOpt, _)) ->
                    match varOpt with
                    | Some var -> $"  %s{n} %s{format var}"
                    | None -> $"  %s{n}")
                (Map.toSeq m)) in
