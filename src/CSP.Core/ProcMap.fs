module CSP.Core.ProcMap

open CSP.Core.Proc
open CSP.Core.ProcMapError
open CSP.Core.Var

type ProcMap<'a> = ProcMap of Map<ProcId, Var list * Proc<'a>>

let from (pm: ((ProcId * string list) * Proc<'a>) seq) : Result<ProcMap<'a>, ProcMapError> =
    pm
    |> Seq.fold
        (fun mRes ((pn, vars), p) ->
            mRes
            |> Result.bind (fun m ->
                if Map.containsKey pn m then
                    Error(DuplicatedProcId(pn))
                else
                    Ok(Map.add pn (List.map Var vars, p) m)))
        (Ok(Map.empty))
    |> Result.map ProcMap

let fold folder s pm =
    match pm with
    | ProcMap m -> Map.fold folder s m

let tryFind pn pm =
    match pm with
    | ProcMap pm -> Map.tryFind pn pm

let formatIds (pm: ProcMap<'a>) : string =
    match pm with
    | ProcMap m ->
        m
        |> Map.toSeq
        |> Seq.map (fun (n, (vars, _)) -> let s = String.concat "" (List.map format vars) in $"  %s{n}%s{s}")
        |> String.concat "\n"

let procIds (pm: ProcMap<'a>) : Set<ProcId> =
    match pm with
    | ProcMap m -> Map.fold (fun keys key _ -> Set.add key keys) Set.empty m
