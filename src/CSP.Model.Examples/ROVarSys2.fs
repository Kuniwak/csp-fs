module CSP.Model.Examples.ROVarSys2

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let evs = setRange (litNat 0u __LINE__) (litNat 6u __LINE__) __LINE__

let unionMap = UnionMap.builtin
let ctorMap = CtorMap.from unionMap |> ResultEx.get CtorMapError.format

let procMap =
    from
        [ (("ROVarSys2", []),
           (interfaceParallel
               (unwind "ROVar" [ litNat 0u __LINE__ ] __LINE__)
               evs
               (interleave
                   (unwind "Reader1" [] __LINE__)
                   (interleave (unwind "Reader2" [] __LINE__) (unwind "Reader3" [] __LINE__) __LINE__)
                   __LINE__)
               __LINE__))
          (("ROVar", [ ("x", tNat) ]),
           (prefix
               (varRef "x" __LINE__)
               (unwind
                   "ROVar"
                   [ ifExpr
                         (less tNat (varRef "x" __LINE__) (litNat 4u __LINE__) __LINE__)
                         (plus tNat (varRef "x" __LINE__) (litNat 1u __LINE__) __LINE__)
                         (litNat 0u __LINE__)
                         __LINE__ ]
                   __LINE__)
               __LINE__))
          (("Reader1", []), prefixRecv evs "x" (stop __LINE__) __LINE__)
          (("Reader2", []), prefixRecv evs "x" (stop __LINE__) __LINE__)
          (("Reader3", []), prefixRecv evs "x" (stop __LINE__) __LINE__) ]
    |> ResultEx.get ProcMapError.format

let genv = Env.empty
