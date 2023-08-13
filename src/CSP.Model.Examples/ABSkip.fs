module CSP.Model.ABSkip

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let tEvent = tUnion "event" []

let unionMap =
    UnionMap.from [ (([], "event"), [ ("a", []); ("b", []) ]) ]
    |> ResultEx.getValue UnionMapError.format

let ctorMap = CtorMap.from unionMap |> ResultEx.getValue CtorMapError.format

let procMap =
    from
        [ ("ABSkip", []), seq (unwind "ASkip" [] __LINE__) (unwind "BSkip" [] __LINE__) __LINE__
          ("ASkip", []), prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__
          ("BSkip", []), prefix (ctor "b" [] __LINE__) (skip __LINE__) __LINE__ ]
    |> ResultEx.getValue ProcMapError.format

let genv = Env.empty
