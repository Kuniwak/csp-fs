module CSP.Core.ProcMap

open CSP.Core.Proc
open CSP.Core.ProcMapError
open CSP.Core.Var

type ProcMap<'a> = ProcMap of Map<ProcId, Var option list * Proc<'a>>

let from (pm: ((ProcId * string list) * Proc<'a>) seq) : Result<ProcMap<'a>, ProcMapError> =
    Result.map
        ProcMap
        (Seq.fold
            (fun mRes ((pn, vars), p) ->
                Result.bind
                    (fun m ->
                        if Map.containsKey pn m then
                            Error(DuplicatedProcId(pn))
                        else
                            let varOpts = List.map (fun var -> if var = "_" then None else Some(Var var)) vars in
                            Ok(Map.add pn (varOpts, p) m))
                    mRes)
            (Ok(Map.empty))
            pm)

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
