open CSP.Core.Val
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.Graph
open CSP.Core.ProcMap

let m: ProcMap<string, string, Unit, string, Unit> =
    Map
        [ ("COUNT",
           (Some "n",
            ExtCh(
                Guard(
                    Less((VarRef "n"), (LitNat 10u)),
                    Prefix("push", Unwind("COUNT", Some(Plus((VarRef "n"), (LitNat 1u)))))
                ),
                Guard(Eq((VarRef "n"), (LitNat 10u)), Prefix("reset", Unwind("COUNT", Some(LitNat 0u))))
            ))) ] in

let env = Map.empty in

printf "%s" (dot 100 m env "COUNT" (Some(VNat 0u)))
