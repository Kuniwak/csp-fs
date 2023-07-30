open CSP.Core
open CSP.Core.ProcEval
open CSP.Core.Visualization.DotLang
open CSP.Model.GHApp

let procEvalCfg: ProcEvalConfig =
    { EvalConfig = { UnivConfig = { NatMax = 3u; ListLenMax = 2u } }
      MaxUnwind = 100 }

let dotCfg: DotConfig =
    { GraphConfig =
        { TransConfig = { ProcEvalConfig = procEvalCfg }
          ProcEvalConfig = procEvalCfg
          SearchConfig = { NodeMax = 10000 } } }

match ProcMap.tryFind "GHSearch" procMap with
| Some(_, p) ->
    printfn $"%s{dot dotCfg procMap ctorMap genv p}"
|
