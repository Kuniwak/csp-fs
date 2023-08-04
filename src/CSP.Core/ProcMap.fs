module CSP.Core.ProcMap

open CSP.Core.Proc
open CSP.Core.ProcMapError
open CSP.Core.Type
open CSP.Core.Expr
open CSP.Core.Util
open CSP.Core.Var

type ProcMap<'a> = ProcMap of Map<ProcId, (Var * Type) list * Proc<'a>>

let from (pm: ((ProcId * (string * Type) list) * Proc<'a>) seq) : Result<ProcMap<'a>, ProcMapError> =
    pm
    |> Seq.map (fun ((pn, args), p) -> (pn, (List.map (fun (var, t) -> (Var var, t)) args, p)))
    |> MapEx.tryFrom
    |> Result.map ProcMap
    |> Result.mapError DuplicatedProcId

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
        |> Seq.map (fun (n, (vars, _)) -> let s = String.concat "" (List.map (format << fst) vars) in $"  %s{n}%s{s}")
        |> String.concat "\n"

let toList (pm: ProcMap<'a>) : (ProcId * ((Var * Type) list * Proc<'a>)) list =
    match pm with
    | ProcMap pm -> Map.toList pm

let procIds (pm: ProcMap<'a>) : Set<ProcId> =
    match pm with
    | ProcMap m -> Map.fold (fun keys key _ -> Set.add key keys) Set.empty m

let formatEntry (x: ProcId * ((Var * Type) list * Proc<unit>)) : string =
    let pn, (vars, p) = x in

    let vars =
        vars
        |> List.map (fun (var, t) -> $"(%s{format var}: %s{Type.format t})")
        |> String.concat " "

    $"%s{pn} %s{vars} = %s{Proc.format noAnnotation p}"

let format (pm: ProcMap<unit>) : string =
    match pm with
    | ProcMap pm -> pm |> Map.toSeq |> Seq.map formatEntry |> String.concat "\n\n"
