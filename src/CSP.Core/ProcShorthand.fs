module CSP.Core.ProcShorthand

open CSP.Core.Ctor
open CSP.Core.LineNum
open CSP.Core.Proc
open CSP.Core.Var

let unwind pn exprOpt = Unwind(pn, exprOpt, unknown)
let stop = Stop(unknown)
let skip = Skip(unknown)
let prefix expr p = Prefix(expr, p, unknown)
let prefixRecv expr var p = PrefixRecv(expr, Var var, p, unknown)
let intCh p1 p2 = IntCh(p1, p2, unknown)
let extCh p1 p2 = ExtCh(p1, p2, unknown)
let seq p1 p2 = Seq(p1, p2, unknown)
let ``if`` expr pThen pElse = If(expr, pThen, pElse, unknown)

let ``match`` expr procMap =
    Match(
        expr,
        Map
            [ for (ctorOpt, vars), p in procMap ->
                  (Option.map Ctor ctorOpt, (List.map (fun var -> if var = "_" then None else Some(Var var)) vars, p)) ],
        unknown
    )

let interfaceParallel p1 expr p2 =
    InterfaceParallel(p1, expr, p2, unknown)

let interleave p1 p2 = Interleave(p1, p2, unknown)
let hide p expr = Hide(p, expr, unknown)
let guard expr p = Guard(expr, p, unknown)
