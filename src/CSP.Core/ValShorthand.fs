module CSP.Core.ValShorthand

open CSP.Core.Ctor
open CSP.Core.Val


let vUnit = VUnit
let vNat = VNat
let vBool = VBool
let vTuple2 vL vR = VTuple(vL, vR)
let vTuple3 v1 v2 v3 = VTuple(v1, VTuple(v2, v3))
let vSet: (Val seq -> Val) = VSet << Set
let vList = VList
let vMap: (Val * Val) seq -> Val = VMap << Map
let vUnion un vs = VUnion(Ctor un, vs)
