module CSP.Core.ProcMap

open CSP.Core.Proc
open CSP.Core.Var

type ProcMap = Map<ProcId, Var option * Proc>
