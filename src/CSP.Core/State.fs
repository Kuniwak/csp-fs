module CSP.Core.State

open CSP.Core.Env
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.Util
open CSP.Core.Val
open CSP.Core.Var

type State =
    | Unwind of ProcId * Val list
    | Stop
    | Skip
    | Prefix of Val * State
    | PrefixRecv of Set<Val> * Env * Var * Proc<unit>
    | IntCh of State * State
    | ExtCh of State * State
    | Seq of State * State
    | InterfaceParallel of State * Set<Val> * State
    | Hide of State * Set<Val>
    | Omega
    | ErrorState of string

let format (genv: Env) (s: State) : string =
    let formatEnv = Env.format genv in

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
        | PrefixRecv(vs, env, var, p) ->
            let s1 =
                vs
                |> Set.toSeq
                |> Seq.map Val.format
                |> String.concat ", "
                |> StringEx.wrapBy "{" "}"

            $"(%s{s1} ? %s{Var.format var} %s{Proc.format noAnnotation p} env=%s{formatEnv env})"
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
