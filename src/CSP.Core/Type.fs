module CSP.Core.Type

open CSP.Core.Val
open Microsoft.FSharp.Core

type Type<'Ctor when 'Ctor: comparison> =
    | TUnit
    | TNat
    | TBool
    | TTuple of Type<'Ctor> * Type<'Ctor>
    | TSet of Type<'Ctor>
    | TList of Type<'Ctor>
    | TMap of Type<'Ctor> * Type<'Ctor>
    | TUnion of string * Map<Ctor<'Ctor>, Type<'Ctor>>
    | TError

let range (n1: uint) (n2: uint) : uint list =
    if n1 > n2 then
        failwith $"the first arg must not be greater than the second one: range {n1} {n2}"

    let rec loop n1 n2 =
        if n1 = n2 then [] else n1 :: (loop (n1 + 1u) n2) in

    loop n1 n2

let natMax = 3u

let powerSet (vs: 'v list) : Set<'v> list =
    let vss = List.map (fun v -> [ Some v; None ]) vs in

    List.fold
        (fun acc vs ->
            List.map
                (fun t ->
                    match t with
                    | Some v, s -> Set.add v s
                    | None, s -> s)
                (List.allPairs vs acc))
        []
        vss


let rec univ (t: Type<'Ctor>) : Val<'Ctor> list =
    match t with
    | TUnit -> [ VUnit ]
    | TNat -> List.map VNat (range 0u natMax)
    | TBool -> [ VBool false; VBool true ]
    | TTuple(l, r) -> List.collect (fun (l, r) -> [ l; r ]) (List.allPairs (univ l) (univ r))
    | TSet t -> List.map VSet (powerSet (univ t))
    | TList t ->
        let vOpts = (None :: (List.map Some (univ t))) in

        List.map
            VList
            (List.fold
                (fun acc vOpt ->
                    match vOpt with
                    | Some v -> List.map (fun vs -> v :: vs) acc
                    | None -> acc)
                ([]: Val<'Ctor> list list)
                vOpts)
    | TMap(tk, tv) ->
        let vOptsList = List.map (fun v -> [ Some v; None ]) (univ tk) in

        List.map
            VMap
            (List.fold
                (fun acc vs ->
                    List.collect
                        (fun t ->
                            match t with
                            | Some kv, m -> List.map (fun vv -> Map.add kv vv m) (univ tv)
                            | None, m -> [ m ])
                        (List.allPairs vs acc))
                []
                vOptsList)

    | TUnion (_, tm) -> List.map VUnion (List.collect (fun (c, t) -> List.map (fun v -> (c, v)) (univ t)) (Map.toList tm))
    | TError -> [VError]
    
let tOption (t: Type<'Ctor>) = TUnion("option", Map [(CtorSome, t); (CtorNone, TUnit)])
let tEither (tl: Type<'Ctor>) (tr: Type<'Ctor>) = TUnion("either", Map [(CtorLeft, tl); (CtorRight, tr)])

let rec format (t: Type<'Ctor>): string =
    match t with
    | TUnit -> "Unit"
    | TNat -> "nat"
    | TBool -> "bool"
    | TTuple(lt, rt) -> $"({format lt} * {format rt})"
    | TSet t -> $"({format t} set)"
    | TList t -> $"({format t} list)"
    | TMap(tk, tv) -> $"(({format tk}, {format tv}) map)"
    | TUnion (n, _) -> n
    | TError -> "error"