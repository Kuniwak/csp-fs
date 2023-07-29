module CSP.Core.TypeShorthand

open CSP.Core.Ctor
open CSP.Core.Type

let tVar n line = TVar(n, line)
let tNat line = TNat(line)
let tBool line = TBool(line)
let tUnit line = TTuple([], line)
let tTuple ts line = TTuple(ts, line)
let tTuple2 t1 t2 line = TTuple([ t1; t2 ], line)
let tTuple3 t1 t2 t3 line = TTuple([ t1; t2; t3 ], line)
let tSet t line = TSet(t, line)
let tList t line = TList(t, line)
let tMap tK tV line = TMap(tK, tV, line)

let tUnion un cm line =
    TUnion(un, Map(Seq.map (fun (ctor, ts) -> (Ctor ctor, ts)) cm), line)

let tOption t line =
    tUnion "option" [ ("Some", [ t ]); ("None", []) ] line

let tEither tL tR line =
    tUnion "either" [ ("Left", [ tL ]); ("Right", [ tR ]) ] line
