module CSP.Core.ProcMap

open CSP.Core.Proc

type ProcMap<'P, 'Var, 'Ctor when 'P: comparison and 'Var: comparison and 'Ctor: comparison> =
    Map<'P, 'Var option * Proc<'P, 'Var, 'Ctor>>
