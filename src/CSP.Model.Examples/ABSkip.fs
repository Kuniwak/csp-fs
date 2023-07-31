module CSP.Model.ABSkip

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let tEvent = tUnion "event" [ ("a", []); ("b", []) ] in

let procMap =
    ResultEx.get
        ProcMapError.format
        (from
            [ ("ABSkip", []), seq (unwind "ASkip" [] __LINE__) (unwind "BSkip" [] __LINE__) __LINE__
              ("ASkip", []), prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__
              ("BSkip", []), prefix (ctor "b" [] __LINE__) (skip __LINE__) __LINE__ ]) in

let ctorMap = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ]) in
let genv = Env.empty in

