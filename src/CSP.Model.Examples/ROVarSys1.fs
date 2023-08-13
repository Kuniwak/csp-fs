module CSP.Model.Examples.ROVarSys1

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let evs = setRange (litNat 0u __LINE__) (litNat 6u __LINE__) __LINE__

let unionMap = UnionMap.builtin
let ctorMap = CtorMap.from unionMap |> ResultEx.getValue CtorMapError.format

let procMap =
    from
        [ (("ROVarSys1", []),
           interfaceParallel
               (unwind "ROVar" [ litNat 0u __LINE__ ] __LINE__)
               evs
               (interfaceParallel
                   (unwind "Reader1" [] __LINE__)
                   evs
                   (interfaceParallel (unwind "Reader2" [] __LINE__) evs (unwind "Reader3" [] __LINE__) __LINE__)
                   __LINE__)
               __LINE__)
          (("ROVar", [ ("n", tNat) ]),
           prefix
               (varRef "n" __LINE__)
               (unwind
                   "ROVar"
                   [ ifExpr
                         (less tNat (varRef "n" __LINE__) (litNat 4u __LINE__) __LINE__)
                         (plus tNat (varRef "n" __LINE__) (litNat 1u __LINE__) __LINE__)
                         (litNat 0u __LINE__)
                         __LINE__ ]
                   __LINE__)
               __LINE__)
          (("Reader1", []), prefixRecv evs "n" (stop __LINE__) __LINE__)
          (("Reader2", []), prefixRecv evs "n" (stop __LINE__) __LINE__)
          (("Reader3", []), prefixRecv evs "n" (stop __LINE__) __LINE__) ]
    |> ResultEx.getValue ProcMapError.format

let genv = Env.empty
