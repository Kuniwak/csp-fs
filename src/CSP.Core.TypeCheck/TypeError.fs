module CSP.Core.TypeError

open CSP.Core.Ctor
open CSP.Core.CtorMapError
open CSP.Core.Expr
open CSP.Core.LineNum
open CSP.Core.Proc
open CSP.Core.TypeCstr
open CSP.Core.TypeEnvError
open CSP.Core.UnionMapError
open CSP.Core.Util
open CSP.Core.Var
open CSP.Core.Type

type TypeError =
    | At of TypeError * string
    | CtorMapError of CtorMapError
    | UnionMapError of UnionMapError
    | TypeNotDerived of TypeCstr * TypeClassName
    | UnionNameMismatch of UnionName * UnionName
    | TypeMismatch of Set<TypeCstr>
    | CtorsMismatch of Set<Ctor> * Set<Ctor>
    | AssociatedValuesLenMismatch of Ctor * Set<int>
    | NoCtors
    | DefaultClauseArgumentsLenMustBe1 of Var option list
    | Recursion of UncertainVarId * TypeCstrUncertainVar.VarMap
    | NotExhausted of Set<Ctor>
    | TypeEnvError of TypeEnvError
    | NoSuchProcess of ProcId
    | ArgumentsLengthMismatch of ProcId * (Var * Type) list * Expr<unit> list
    | TypeArgumentsLengthMismatch of Set<TypeCstr list>

let atLine (line: LineNum) (err: TypeError) : TypeError = At(err, $"line %s{line}")

let rec unwrapTypeError (terr: TypeError) : TypeError =
    match terr with
    | At(terr, _) -> unwrapTypeError terr
    | _ -> terr

let format (terr: TypeError) : string =
    let rec format terr =
        match terr with
        | At(terr, hint) -> $"%s{format terr}\n\tat %s{hint}"
        | CtorMapError(err) -> CtorMapError.format err
        | UnionMapError(err) -> UnionMapError.format err
        | TypeNotDerived(t, tcClassName) -> $"type not derived %s{tcClassName}: %s{TypeCstr.format t}"
        | UnionNameMismatch(un1, un2) -> $"union name mismatch: %s{un1} vs %s{un2}"
        | TypeMismatch(s) -> let s = s |> Seq.map TypeCstr.format |> String.concat " vs " in $"type mismatch: %s{s}"
        | AssociatedValuesLenMismatch(ctor, s) ->
            let s = s |> Seq.map (fun x -> $"%d{x}") |> String.concat " vs " in
            $"length of associated values mismatch: %s{Ctor.format ctor} %s{s}"
        | CtorsMismatch(s1, s2) ->
            let s1 = String.concat ", " (Seq.map Ctor.format s1) in
            let s2 = String.concat ", " (Seq.map Ctor.format s2) in
            $"constructors mismatch: {{%s{s1}}} vs {{%s{s2}}}"
        | NoCtors -> "match needs at least one constructor"
        | NotExhausted(missing) ->
            let missing = String.concat ", " (Seq.map Ctor.format (Set.toSeq missing)) in
            $"not exhausted match: {{%s{missing}}}"
        | DefaultClauseArgumentsLenMustBe1(vars) ->
            let s =
                vars
                |> Seq.map (fun varOpt ->
                    match varOpt with
                    | Some var -> Var.format var
                    | None -> "_")
                |> String.concat " " in

            $"length of arguments for default clause must be 1, but got: %s{s}"
        | Recursion(u, m) ->
            let s =
                m.Map
                |> Map.toSeq
                |> Seq.map (fun (u, t) -> $"  %s{TypeCstr.format (TCUncertain u)} -> %s{TypeCstr.format t}")
                |> String.concat "\n" in

            $"type recursion: %s{TypeCstr.format (TCUncertain u)} in\n%s{s}"
        | TypeEnvError(err) -> TypeEnvError.format err
        | NoSuchProcess(pn) -> $"no such process: %s{pn}"
        | ArgumentsLengthMismatch(pn, vars, exprs) ->
            let s1 =
                vars
                |> List.map (fun (var, t) -> $" (%s{Var.format var}: %s{Type.format t})")
                |> String.concat "" in

            let s2 = exprs |> List.map (Expr.format noAnnotation) |> String.concat " " in
            $"arguments length mismatch: (%s{pn}%s{s1}) vs (%s{s2})"
        | TypeArgumentsLengthMismatch(s) ->
            let s =
                s
                |> Seq.map (fun tcs -> tcs |> Seq.map TypeCstr.format |> String.concat " " |> StringEx.wrapBy "(" ")")
                |> String.concat " vs " in

            $"type arguments length mismatch: %s{s}"

    $"""type error: {format terr}"""
