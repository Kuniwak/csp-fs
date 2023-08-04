module CSP.Model.Examples.Rand2

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.Util

let unionMap = UnionMap.builtin

let procMap =
    from
        [ (("P", []),
           intCh
               (prefix (litNat 1u __LINE__) (unwind "P" [] __LINE__) __LINE__)
               (prefix (litNat 2u __LINE__) (unwind "P" [] __LINE__) __LINE__)
               __LINE__) ]
    |> ResultEx.get ProcMapError.format

let genv = Env.empty
