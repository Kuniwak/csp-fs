module CSP.Model.Examples.Count

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let unionMap =
    UnionMap.from [ (([], "event"), [ ("push", []); ("reset", []) ]) ]
    |> ResultEx.getValue UnionMapError.format

let ctorMap = CtorMap.from unionMap |> ResultEx.getValue CtorMapError.format

let procMap =
    from
        [ (("COUNT", [ ("n", tNat) ]),
           extCh
               (guard
                   (less tNat (varRef "n" __LINE__) (litNat 10u __LINE__) __LINE__)
                   (prefix
                       (ctor "push" [] __LINE__)
                       (unwind "COUNT" [ plus tNat (varRef "n" __LINE__) (litNat 1u __LINE__) __LINE__ ] __LINE__)
                       __LINE__)
                   __LINE__)
               (guard
                   (eq tNat (varRef "n" __LINE__) (litNat 10u __LINE__) __LINE__)
                   (prefix (ctor "reset" [] __LINE__) (unwind "COUNT" [ litNat 0u __LINE__ ] __LINE__) __LINE__)
                   __LINE__)
               __LINE__) ]
    |> ResultEx.getValue ProcMapError.format


let genv = Env.empty
