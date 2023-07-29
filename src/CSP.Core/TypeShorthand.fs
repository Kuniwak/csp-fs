module CSP.Core.TypeShorthand

open CSP.Core.Ctor
open CSP.Core.Type

let tVar n = TVar(n)
let tNat = TNat
let tBool = TBool
let tUnit = TUnit
let tTuple2 t1 t2 = TTuple(t1, t2)
let tTuple3 t1 t2 t3 = TTuple(t1, TTuple(t2, t3))
let tSet t = TSet(t)
let tList t = TList(t)
let tMap tK tV = TMap(tK, tV)

let tUnionM un cm = TUnion(un, cm)

let tUnion un cm =
    tUnionM un (Map(Seq.map (fun (ctor, ts) -> (Ctor ctor, ts)) cm))

let tOption t =
    tUnion "option" [ ("Some", [ t ]); ("None", []) ]

let tEither tL tR =
    tUnion "either" [ ("Left", [ tL ]); ("Right", [ tR ]) ]
