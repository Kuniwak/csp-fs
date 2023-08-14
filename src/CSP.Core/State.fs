module CSP.Core.State

open CSP.Core.Util
open CSP.Core.Proc
open CSP.Core.Val

type State<'a when 'a : comparison> =
    | Unwind of ProcId * Val list
    | Stop
    | Skip
    | Prefix of Val * State<'a>
    | IntCh of State<'a> * State<'a>
    | ExtCh of State<'a> * State<'a>
    | Seq of State<'a> * State<'a>
    | InterfaceParallel of State<'a> * Set<Val> * State<'a>
    | Hide of State<'a> * Set<Val>
    | Omega
    | ErrorState of string

let format (s: State<'a>) : string =
    let rec format s =
        match s with
        | Unwind(pn, vs) ->
            if List.isEmpty vs then
                $"%s{pn}"
            else
                let s = vs |> Seq.map Val.format |> String.concat " "
                $"(%s{pn} %s{s})"

        | Stop -> "STOP"
        | Skip -> "SKIP"
        | Prefix(v, s) -> $"(%s{Val.format v} → {format s})"
        | IntCh(s1, s2) -> $"({format s1} ⨅ {format s2})"
        | ExtCh(s1, s2) -> $"({format s1} □ {format s2})"
        | Seq(s1, s2) -> $"({format s1} ; {format s2})"
        | InterfaceParallel(s1, vs, s2) ->
            let s =
                vs
                |> Set.toSeq
                |> Seq.map Val.format
                |> String.concat ", "
                |> StringEx.wrapBy "{" "}"

            $"({format s1} ⟦%s{s}⟧ {format s2})"
        | Hide(s, vs) ->
            let vs =
                vs
                |> Set.toSeq
                |> Seq.map Val.format
                |> String.concat ", "
                |> StringEx.wrapBy "{" "}"

            $"({format s} \\\\ %s{vs})"
        | Omega -> "Ω"
        | ErrorState msg -> $"(ERROR: %s{msg})"

    format s
