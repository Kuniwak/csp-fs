module CSP.Core.TypeShorthand

open CSP.Core.Ctor
open CSP.Core.Type

let tVar n = TVar n
let tNat = TNat
let tBool = TBool
let tUnit = TTuple([])
let tTuple ts = TTuple(ts)
let tTuple2 t1 t2 = TTuple([ t1; t2 ])
let tTuple3 t1 t2 t3 = TTuple([ t1; t2; t3 ])
let tSet t = TSet t
let tList t = TList t
let tMap tK tV = TMap(tK, tV)

let tUnion un cm =
    TUnion(un, Map(Seq.map (fun (ctor, ts) -> (Ctor ctor, ts)) cm))
