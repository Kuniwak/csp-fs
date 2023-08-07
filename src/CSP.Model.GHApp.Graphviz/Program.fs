open CSP.Core.ProcEval
open CSP.Core.Univ
open CSP.Core.Visualization.DotLang

let univCfg: UnivConfig = { NatMax = 3u; ListLenMax = 2u }
let procEvalCfg: ProcEvalConfig = { EvalConfig = { UnivConfig = univCfg } }

let dotCfg: DotConfig =
    { GraphConfig =
        { TransConfig = { ProcEvalConfig = procEvalCfg }
          ProcEvalConfig = procEvalCfg
          SearchConfig = { NodeMax = 10000 }
          NamedConfig =
            { UnivConfig = univCfg
              ProcEvalConfig = procEvalCfg } } }

match dot dotCfg procMap ctorMap genv "GHStar" [ vSet [] ] with
| Ok(s) -> printfn $"%s{s}"
| Error(err) -> printfn $"%s{ProcEvalError.format err}"
