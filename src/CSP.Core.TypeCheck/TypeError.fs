module CSP.Core.TypeError

open CSP.Core.Ctor
open CSP.Core.LineNum
open CSP.Core.TypeCstr
open CSP.Core.TypeEnvError
open CSP.Core.Var
open CSP.Core.Type

type TypeError =
    | At of TypeError * string
    | TypeNotDerived of Type * TypeClassName
    | UnionNameMismatch of UnionName * UnionName
    | NotUnion of TypeCstr
    | NotTuple of TypeCstr
    | TupleIndexOutOfBounds of TypeCstr * uint
    | TupleLengthMismatch of Set<TypeCstr list>
    | TypeMismatch of Set<TypeCstr>
    | NoSuchCtor of Ctor
    | CtorsMismatch of Set<Ctor> * Set<Ctor>
    | AssociatedValuesLenMismatch of Ctor * Set<int>
    | EmptyMatch
    | DefaultClauseArgumentsLenMustBe1 of Var option list
    | Recursion of UncertainVarId * TypeCstrUncertainVar.VarMap
    | TypeEnvError of TypeEnvError

let atLine (line: LineNum) (err: TypeError) : TypeError = At(err, $"line %s{line}")

let rec unwrapTypeError (terr: TypeError) : TypeError =
    match terr with
    | At(terr, _) -> unwrapTypeError terr
    | _ -> terr

let format (terr: TypeError) : string =
    let rec format terr =
        match terr with
        | At(terr, hint) -> $"%s{format terr}\n\tat %s{hint}"
        | TypeNotDerived(t, tcClassName) -> $"type not derived %s{tcClassName}: %s{Type.format t}"
        | UnionNameMismatch(un1, un2) -> $"union name mismatch: %s{un1} vs %s{un2}"
        | TypeMismatch(s) ->
            let s = String.concat " vs " (List.map TypeCstr.format (Set.toList s)) in $"type mismatch: %s{s}"
        | NotUnion t -> $"not union: %s{TypeCstr.format t}"
        | NotTuple t -> $"not tuple: %s{TypeCstr.format t}"
        | TupleIndexOutOfBounds(t, idx) -> $"tuple index out of bounds: %s{TypeCstr.format t} at %d{idx}"
        | TupleLengthMismatch(s) ->
            let s =
                String.concat " vs " (List.map (fun tcs -> TypeCstr.format (TCTuple tcs)) (Set.toList s))

            $"tuple length mismatch: %s{s}"
        | AssociatedValuesLenMismatch(ctor, s) ->
            let s = String.concat " vs " (Seq.map (fun n -> $"%d{n}") (Set.toSeq s)) in
            $"length of associated values mismatch: %s{Ctor.format ctor} (%s{s})"
        | NoSuchCtor ctor -> $"no such data constructor: %s{Ctor.format ctor}"
        | CtorsMismatch(s1, s2) ->
            let s1 = String.concat ", " (Seq.map Ctor.format s1) in
            let s2 = String.concat ", " (Seq.map Ctor.format s2) in
            $"constructors mismatch: {{%s{s1}}} vs {{%s{s2}}}"
        | EmptyMatch -> "match must be have at least one clause that including a default clause"
        | DefaultClauseArgumentsLenMustBe1(vars) ->
            let s =
                String.concat
                    ","
                    (List.map
                        (fun varOpt ->
                            match varOpt with
                            | Some var -> Var.format var
                            | None -> "_")
                        vars) in

            $"length of arguments for default clause must be 1, but got: [%s{s}]"
        | Recursion(u, m) ->
            let s =
                String.concat
                    "\n"
                    (List.map
                        (fun (u, t) -> $"  %s{TypeCstr.format (TCUncertain u)} -> %s{TypeCstr.format t}")
                        (Map.toList m.Map)) in

            $"type recursion: %s{TypeCstr.format (TCUncertain u)} in\n%s{s}"
        | TypeEnvError(err) -> TypeEnvError.format err

    $"""type error: {format terr}"""

