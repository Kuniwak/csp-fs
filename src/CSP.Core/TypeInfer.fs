module CSP.Core.TypeInfer

open CSP.Core.CtorMap
open CSP.Core.Expr
open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.TypeEnv

let occChk (m: Map<uint, TypeCstr>) (u: uint) (t: TypeCstr) : bool =
    let rec occChk visited t =
        match t with
        | TCVar u' ->
            if Set.contains u' visited then
                true
            else
                match Map.tryFind u' m with
                | Some t' -> occChk (Set.add u' visited) t'
                | None -> false
        | TCUnit -> false
        | TCBool -> false
        | TCNat -> false
        | TCTuple(tL, tR) -> occChk visited tL && occChk visited tR
        | TCUnion(_, tV) -> occChk visited tV
        | TCSet t -> occChk visited t
        | TCList t -> occChk visited t
        | TCMap(tK, tV) -> occChk visited tK && occChk visited tV
        | TCError -> false in

    occChk (Set[u]) t

let unify (m: Map<uint, TypeCstr>) (t1: TypeCstr) (t2: TypeCstr) : Result<TypeCstr * Map<uint, TypeCstr>, string> =
    let rec unify m tc1 tc2 =
        match tc1, tc2 with
        | TCVar n1, TCVar n2 ->
            if occChk m n1 (TCVar n2) then
                Error $"recursive type: {n1}"
            else
                Ok(tc2, Map.add n1 tc2 m)
        | TCVar n, _ -> Ok(tc2, Map.add n tc2 m)
        | _, TCVar n -> Ok(tc1, Map.add n tc1 m)
        | TCUnit, TCUnit -> Ok(TCUnit, m)
        | TCBool, TCBool -> Ok(TCUnit, m)
        | TCNat, TCNat -> Ok(TCNat, m)
        | TCError, TCError -> Ok(TCError, m)
        | TCTuple(tcL1, tcR1), TCTuple(tcL2, tcR2) ->
            // NOTE: Do not use Result.mapError. It make hard to read.
            match unify m tcL1 tcL2 with
            | Error s -> Error $"{s}:\n\tin the left of {format tc1} vs {format tc2}"
            | Ok(tL, m) ->
                match unify m tcR1 tcR2 with
                | Error err -> Error $"{err}:\n\tin the right of {format tc1} vs {format tc2}"
                | Ok(tR, m) -> Ok(TCTuple(tL, tR), m)
        | TCUnion(un1, tcV1), TCUnion(un2, tcV2) when un1 = un2 ->
            // NOTE: Do not use Result.mapError. It make hard to read.
            match unify m tcV1 tcV2 with
            | Error err -> Error $"{err}:\n\tin the type argument of {format tc1} vs {format tc2}"
            | Ok(t, m) -> Ok(TCUnion(un1, t), m)
        | TCUnion _, TCUnion _ -> Error $"union name mismatch: {format tc1} vs {format tc2}"
        | TCList tcV1, TCList tcV2 ->
            match unify m tcV1 tcV2 with
            | Error err -> Error $"{err}:\n\tin the element type of {format tc1} vs {format tc2}"
            | Ok(tcV, m) -> Ok(TCList(tcV), m)
        | TCSet tcV1, TCSet tcV2 ->
            match unify m tcV1 tcV2 with
            | Error err -> Error $"{err}:\n\tin the element type of {format tc1} vs {format tc2}"
            | Ok(tcV, m) -> Ok(TCSet(tcV), m)
        | TCMap(tcK1, tcV1), TCMap(tcK2, tcV2) ->
            // NOTE: Do not use Result.mapError. It make hard to read.
            match unify m tcK1 tcK2 with
            | Error s -> Error $"{s}:\n\tin the key type of {format tc1} vs {format tc2}"
            | Ok(tcK, m) ->
                match unify m tcV1 tcV2 with
                | Error err -> Error $"{err}:\n\tin the value type of {format tc1} vs {format tc2}"
                | Ok(tcV, m) -> Ok(TCMap(tcK, tcV), m)
        | _, _ -> Error $"type mismatch {format tc1} vs {format tc2}"

    unify m t1 t2

let infer
    (cm: CtorMap)
    (n: uint)
    (m: Map<uint, TypeCstr>)
    (tenv: TypeEnv)
    (expr: Expr)
    : Result<Map<uint, TypeCstr> * uint * TypeCstr, string> =
    let rec infer n m tenv expr =
        match expr with
        | LitUnit _ -> Ok(m, n, TCUnit)
        | LitTrue _ -> Ok(m, n, TCBool)
        | LitFalse _ -> Ok(m, n, TCBool)
        | LitNat _ -> Ok(m, n, TCNat)
        | LitEmpty(t, line) ->
            match t with
            | TSet t -> Ok(m, n, TCSet(ofType t))
            | TList t -> Ok(m, n, TCList(ofType t))
            | TMap(tK, tV) -> Ok(m, n, TCMap(ofType tK, ofType tV))
            | _ -> Error $"type is not derived Empty at line {line}: {Type.format t}"
        | LitError _ -> Ok(m, n, TCError)
        | Union(ctor, expr, line) ->
            let un, tV = Map.find ctor cm in
            let tcV = ofType tV in
            match infer n m tenv expr with
            | Ok(m, n, tcV')  ->
                match unify m tcV tcV' with
                | Ok(tcV, m) -> Ok(m, n, TCUnion(UNName un, tcV))
                | Error s -> Error $"type error at line {line}: {s}"
            | Error s -> Error $"type error at line {line}: {s}"
        | Throw _ -> Ok(m, n, TCError)
        | If(exprCond, exprThen, exprElse, line) ->
            match infer n m tenv exprCond with
            | Ok(m, n, tcCond) ->
                match unify m tcCond TCBool with
                | Ok(_, m) ->
                    match infer n m tenv exprThen with
                    | Ok(m, n, tcThen) ->
                        match infer n m tenv exprElse with
                        | Ok(m, n, tcElse) ->
                            match unify m tcThen tcElse with
                            | Ok(tcIf, m) -> Ok(m, n, tcIf)
                            | Error s -> Error $"type error at line {line}: {s}"
                        | Error s -> Error $"type error at line {line}: {s}"
                    | Error s -> Error $"type error at line {line}: {s}"
                | Error s -> Error $"type error at line {line}: {s}"
            | Error s -> Error $"type error at line {line}: {s}"
        | Match(exprUnion, exprMap, defExpr, line) ->
            match infer n m tenv exprUnion with
            | Ok(m, n, tcUnion) ->
                match unify m tcUnion (TCUnion(UNVar 0, TCVar n)) with
                | Ok(tcUnion, m) ->
                    let n = n + 1u in
                    Map.fold
                        (fun acc ctor (var, expr) ->
                            
                        )
                

    infer n m tenv expr
