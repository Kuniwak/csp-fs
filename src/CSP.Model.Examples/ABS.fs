module CSP.Model.Examples.ABS

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.Util

let unionMap =
    UnionMap.from [ (([], "event"), [ ("a", []); ("b", []); ("s", []) ]) ]
    |> ResultEx.getValue UnionMapError.format

let ctorMap = CtorMap.from unionMap |> ResultEx.getValue CtorMapError.format

let procMap =
    from
        [ (("ABS", []),
           (extCh
               (intCh
                   (prefix (ctor "a" [] __LINE__) (unwind "ABS" [] __LINE__) __LINE__)
                   (prefix (ctor "b" [] __LINE__) (unwind "ABS" [] __LINE__) __LINE__)
                   __LINE__)
               (prefix (ctor "s" [] __LINE__) (stop __LINE__) __LINE__)
               __LINE__)) ]
    |> ResultEx.getValue ProcMapError.format


let genv = Env.empty
