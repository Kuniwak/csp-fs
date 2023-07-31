module CSP.Model.Examples.ParABC

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let tEvent = tUnion "event" [ ("a", []); ("b", []); ("c", []); ("d", []) ]

let procMap =
    ResultEx.get
        ProcMapError.format
        (from
            [ (("ParABC", []),
               interleave
                   (prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__)
                   (interleave
                       (prefix (ctor "b" [] __LINE__) (skip __LINE__) __LINE__)
                       (prefix (ctor "c" [] __LINE__) (skip __LINE__) __LINE__)
                       __LINE__)
                   __LINE__)
              (("P", []),
               seq (unwind "ParABC" [] __LINE__) (prefix (ctor "d" [] __LINE__) (skip __LINE__) __LINE__) __LINE__) ])

let ctorMap = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])
let genv = Env.empty
