module CSP.Core.ProcMap

open CSP.Core.Proc
open CSP.Core.Var

type ProcMap = Map<ProcId, Var option * Proc>

let from (pm: ((ProcId * Var option) * Proc) seq) : ProcMap =
    Map [ for (pn, optVar), p in pm -> (pn, (optVar, p)) ]
