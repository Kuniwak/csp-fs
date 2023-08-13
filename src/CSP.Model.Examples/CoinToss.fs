module CSP.Model.Examples.CoinToss

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let unionMap =
    UnionMap.from [ (([], "event"), [ ("toss", []); ("heads", []); ("tails", []); ("right", []); ("left", []) ]) ]
    |> ResultEx.getValue UnionMapError.format

let ctorMap = CtorMap.from unionMap |> ResultEx.getValue CtorMapError.format

let tEvent = tUnion "event" []

let procMap =
    from
        [ (("Coin", []), prefix (ctor "toss" [] __LINE__) (unwind "Coin'" [] __LINE__) __LINE__)
          (("Coin'", []),
           intCh
               (prefix (ctor "heads" [] __LINE__) (unwind "Coin" [] __LINE__) __LINE__)
               (prefix (ctor "tails" [] __LINE__) (unwind "Coin" [] __LINE__) __LINE__)
               __LINE__)
          (("Man", []), prefix (ctor "toss" [] __LINE__) (unwind "Man'" [] __LINE__) __LINE__)
          (("Man'", []),
           extCh
               (prefix
                   (ctor "heads" [] __LINE__)
                   (prefix (ctor "left" [] __LINE__) (unwind "Man" [] __LINE__) __LINE__)
                   __LINE__)
               (prefix
                   (ctor "tails" [] __LINE__)
                   (prefix (ctor "right" [] __LINE__) (unwind "Man" [] __LINE__) __LINE__)
                   __LINE__)
               __LINE__)
          (("CoinToss", []),
           interfaceParallel
               (unwind "Coin" [] __LINE__)
               (setInsert
                   (ctor "toss" [] __LINE__)
                   (setInsert
                       (ctor "heads" [] __LINE__)
                       (setInsert (ctor "tails" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
                       __LINE__)
                   __LINE__)
               (unwind "Man" [] __LINE__)
               __LINE__) ]
    |> ResultEx.getValue ProcMapError.format

let genv = Env.empty
