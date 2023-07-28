module CSP.Core.ValShorthand

open CSP.Core.Ctor
open CSP.Core.Val

let vNat = VNat
let vBool = VBool
let vTuple = VTuple
let vTuple2 vL vR = VTuple([ vL; vR ])
let vSet = VSet
let vList = VList
let vMap: (Val * Val) seq -> Val = VMap << Map
let vUnion un vs = VUnion(Ctor un, vs)
