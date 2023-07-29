module CSP.Core.ProcShorthand

open CSP.Core.Ctor
open CSP.Core.Proc
open CSP.Core.Var

let unwind pn exprOpt line = Unwind(pn, exprOpt, line)
let stop line = Stop(line)
let skip line = Skip(line)
let prefix expr p line = Prefix(expr, p, line)
let prefixRecv expr var p line = PrefixRecv(expr, Var var, p, line)
let intCh p1 p2 line = IntCh(p1, p2, line)
let extCh p1 p2 line = ExtCh(p1, p2, line)
let seq p1 p2 line = Seq(p1, p2, line)
let ``if`` expr pThen pElse line = If(expr, pThen, pElse, line)

let ``match`` expr procMap line =
    Match(
        expr,
        Map
            [ for (ctorOpt, vars), p in procMap ->
                  (Option.map Ctor ctorOpt, (List.map (fun var -> if var = "_" then None else Some(Var var)) vars, p)) ],
        line
    )

let interfaceParallel p1 expr p2 line =
    InterfaceParallel(p1, expr, p2, line)

let interleave p1 p2 line = Interleave(p1, p2, line)
let hide p expr line = Hide(p, expr, line)
let guard expr p line = Guard(expr, p, line)
