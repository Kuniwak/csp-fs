module CSP.Model.Examples.Hide3

open CSP.Core
open CSP.Core.ProcMap
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util

let tEvent = tUnion "event" []

let unionMap =
    UnionMap.from [ (([], "event"), [ ("a", []) ]) ]
    |> ResultEx.getValue UnionMapError.format

let ctorMap = CtorMap.from unionMap |> ResultEx.getValue CtorMapError.format

let procMap =
    from
        [ ("P", []),
          hide
              (prefix (ctor "a" [] __LINE__) (skip __LINE__) __LINE__)
              (setInsert (ctor "a" [] __LINE__) (litEmpty (tSet tEvent) __LINE__) __LINE__)
              __LINE__ ]
    |> ResultEx.getValue ProcMapError.format

let genv = Env.empty
