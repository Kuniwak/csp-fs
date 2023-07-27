module CSP.Core.TypeCstrShorthand

open CSP.Core.Ctor
open CSP.Core.TypeCstr

let tcUncertain n = TCUncertain (UncertainVarId n)
let tcNat = TCNat
let tcBool = TCBool
let tcUnit = TCTuple([])
let tcTuple ts = TCTuple(ts)
let tcTuple2 t1 t2 = TCTuple([ t1; t2 ])
let tcTuple3 t1 t2 t3 = TCTuple([ t1; t2; t3 ])
let tcSet t = TCSet t
let tcList t = TCList t
let tcMap tK tV = TCMap(tK, tV)

let tcUnion un cm = TCUnion(un, Map(Seq.map (fun (ctor, ts) -> (Ctor ctor, ts)) cm))
