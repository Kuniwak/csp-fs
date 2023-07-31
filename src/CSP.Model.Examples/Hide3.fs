module CSP.Model.Examples.Hide3

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let tEvent = tUnion "event" [ ("a", []) ]

let procMap =
    ResultEx.get
        ProcMapError.format
        (from
            [ ("P", []),
              hide
                  (prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__)
                  (setInsert (ctor "a" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
                  __LINE__ ])

let ctorMap = ResultEx.get CtorMapError.format (CtorMap.from [ tEvent ])
let genv = Env.empty
