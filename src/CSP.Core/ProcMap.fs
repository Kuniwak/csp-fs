module CSP.Core.ProcMap

open CSP.Core.Proc
open CSP.Core.Var

type ProcMap<'a> = ProcMap of Map<ProcId, Var option list * Proc<'a>>

let from (pm: ((ProcId * string list) * Proc<'a>) seq) : ProcMap<'a> =
    ProcMap(
        Map
            [ for (pn, vars), p in pm -> (pn, (List.map (fun var -> if var = "_" then None else Some(Var var)) vars, p)) ]
    )

let fold folder s pm =
    match pm with
    | ProcMap m -> Map.fold folder s m

let tryFind pn pm =
    match pm with
    | ProcMap m -> Map.tryFind pn m

let formatIds (pm: ProcMap<'a>) : string =
    match pm with
    | ProcMap m ->
        String.concat
            "\n"
            (Seq.map
                (fun (n, (varOpts, _)) ->
                    let s =
                        String.concat
                            ""
                            (List.map
                                (fun varOpt ->
                                    match varOpt with
                                    | Some var -> $" %s{format var}"
                                    | None -> " _")
                                varOpts)

                    $"  %s{n}%s{s}")
                (Map.toSeq m))

let procIds (pm: ProcMap<'a>) : Set<ProcId> =
    match pm with
    | ProcMap m -> Map.fold (fun keys key _ -> Set.add key keys) Set.empty m
