module CSP.Core.TypeCstrShorthand

open CSP.Core.TypeCstr

let tcUncertain n = TCUncertain(UncertainVarId n)
let tcNat = TCNat
let tcBool = TCBool
let tcUnit = TCUnit
let tcTuple2 tc1 tc2 = TCTuple(tc1, tc2)
let tcTuple3 tc1 tc2 tc3 = TCTuple(tc1, TCTuple(tc2, tc3))
let tcSet tc = TCSet tc
let tcList tc = TCList tc
let tcMap tcK tcV = TCMap(tcK, tcV)
let tcUnion un tcs = TCUnion(un, tcs)
