module CSP.Model.Examples.ABS

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let tEvent = tUnion "event" [ ("a", []); ("b", []); ("s", []) ]

let procMap =
    ResultEx.get
        ProcMapError.format
        (from
            [ (("ABS", []),
               (extCh
                   (intCh
                       (prefix (ctor "a" [] __LINE__) (unwind "ABS" [] __LINE__) __LINE__)
                       (prefix (ctor "b" [] __LINE__) (unwind "ABS" [] __LINE__) __LINE__)
                       __LINE__)
                   (prefix (ctor "s" [] __LINE__) (stop __LINE__) __LINE__)
                   __LINE__)) ])

let ctorMap = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])
let genv = Env.empty
