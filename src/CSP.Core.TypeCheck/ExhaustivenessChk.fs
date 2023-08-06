module CSP.Core.ExhaustivenessChk

open CSP.Core
open CSP.Core.Expr
open CSP.Core.UnionMap
open CSP.Core.CtorMap
open CSP.Core.Ctor
open CSP.Core.Type
open CSP.Core.TypeError
open CSP.Core.Var

let exhaustivenessCheck
    (um: UnionMap)
    (cm: CtorMap)
    (exprMap: Map<Ctor option, Var option list * 'a>)
    : Result<UnionName * TVarId list * Map<Ctor, Type list>, TypeError> =
    tryAnyCtor exprMap
    |> Option.map Ok
    |> Option.defaultValue (Error(NoCtors))
    |> Result.bind (fun anyCtor -> tryFindOnlyUnionName anyCtor cm |> Result.mapError CtorMapError)
    |> Result.bind (fun un ->
        UnionMap.tryFind un um
        |> Result.mapError UnionMapError
        |> Result.bind (fun (tVars, cm) ->
            let actualCtors = exprMap |> Map.keys |> Seq.collect Option.toList |> Set.ofSeq in
            let expectedCtors = cm |> Map.keys |> Set.ofSeq in

            if Set.isSubset expectedCtors actualCtors || Map.containsKey None exprMap then
                Ok(un, tVars, cm)
            else
                Error(NotExhausted(Set.difference expectedCtors actualCtors))))
