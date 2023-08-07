open CSP.Core.CLI
open CSP.Core.ProcEval

let procEvalCfg: ProcEvalConfig =
    { EvalConfig = { UnivConfig = { NatMax = 3u; ListLenMax = 2u } } }

let interpreterCfg: InterpreterConfig =
    { TransConfig = { ProcEvalConfig = procEvalCfg }
      ProcEvalConfig = procEvalCfg }

start interpreterCfg procMap ctorMap genv "GHStar" [ vSet (Set.empty) ]
