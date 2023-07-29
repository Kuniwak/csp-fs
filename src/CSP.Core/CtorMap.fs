module CSP.Core.CtorMap

open CSP.Core.Ctor
open CSP.Core.Type

type CtorMap = Map<Ctor, UnionName * Map<Ctor, Type list>>

let from (ts: Type seq) : CtorMap =
    Map(
        Seq.collect
            (fun t ->
                match t with
                | TUnion(un, cm, _) -> Seq.map (fun (ctor, _) -> (ctor, (un, cm))) (Map.toSeq cm)
                | _ -> Seq.empty)
            ts
    )

let empty: CtorMap = Map.empty
