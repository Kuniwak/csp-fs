module CSP.Model.Examples.ParABC

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.Util

let unionMap =
    UnionMap.from [ (([], "event"), [ ("a", []); ("b", []); ("c", []); ("d", []) ]) ]
    |> ResultEx.get UnionMapError.format

let ctorMap = CtorMap.from unionMap |> ResultEx.get CtorMapError.format

let procMap =
    from
        [ (("ParABC", []),
           interleave
               (prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__)
               (interleave
                   (prefix (ctor "b" [] __LINE__) (skip __LINE__) __LINE__)
                   (prefix (ctor "c" [] __LINE__) (skip __LINE__) __LINE__)
                   __LINE__)
               __LINE__)
          (("P", []),
           seq (unwind "ParABC" [] __LINE__) (prefix (ctor "d" [] __LINE__) (skip __LINE__) __LINE__) __LINE__) ]
    |> ResultEx.get ProcMapError.format

let genv = Env.empty
