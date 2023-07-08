module CSP.Core.ProcMap

open CSP.Core.Proc

type ProcMap<'P, 'Ev, 'Ch, 'Var, 'Ctor
    when 'P: comparison and 'Ev: comparison and 'Var: comparison and 'Ctor: comparison and 'Ch: comparison> =
    Map<'P, 'Var option * Proc<'P, 'Ev, 'Ch, 'Var, 'Ctor>>
