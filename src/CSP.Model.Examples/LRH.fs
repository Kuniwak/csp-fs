module CSP.Model.Examples.LRH

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let unionMap =
    UnionMap.from [ (([], "event"), [ ("blue", []); ("red", []); ("sync", []) ]) ]
    |> ResultEx.get UnionMapError.format
let ctorMap = CtorMap.from unionMap |> ResultEx.get CtorMapError.format

let tEvent = tUnion "event" []

let procMap =
    from
        [ (("LRH", []),
           hide
               (interfaceParallel
                   (unwind "Left" [] __LINE__)
                   (setInsert (ctor "sync" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
                   (unwind "Right" [] __LINE__)
                   __LINE__)
               (setInsert (ctor "sync" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
               __LINE__)
          (("Left", []),
           prefix
               (ctor "blue" [] __LINE__)
               (prefix (ctor "sync" [] __LINE__) (unwind "Left" [] __LINE__) __LINE__)
               __LINE__)
          (("Right", []),
           prefix
               (ctor "red" [] __LINE__)
               (prefix (ctor "sync" [] __LINE__) (unwind "Right" [] __LINE__) __LINE__)
               __LINE__) ]
    |> ResultEx.get ProcMapError.format


let genv = Env.empty
