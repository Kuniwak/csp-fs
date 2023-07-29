module CSP.Core.CtorMap

open CSP.Core.Ctor
open CSP.Core.CtorMapError
open CSP.Core.Type

type CtorMap = Map<Ctor, UnionName * Map<Ctor, Type list>>

let from (ts: Type seq) : Result<CtorMap, CtorMapError> =
    Seq.fold
        (fun mRes t ->
            match t with
            | TUnion(un, cm) ->
                Map.fold
                    (fun mRes ctor _ ->
                        Result.bind
                            (fun m ->
                                if Map.containsKey ctor m then
                                    Error(DuplicateCtor(ctor))
                                else
                                    Ok(Map.add ctor (un, cm) m))
                            mRes)
                    mRes
                    cm
            | _ -> mRes)
        (Ok(Map.empty))
        ts

let empty: CtorMap = Map.empty
