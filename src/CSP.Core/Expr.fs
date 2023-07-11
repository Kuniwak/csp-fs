module CSP.Core.Expr

open CSP.Core.Type
open CSP.Core.Val
open CSP.Core.Env

type Expr<'Var, 'Ctor when 'Var: comparison and 'Ctor: comparison> =
    | Lit of Val<'Ctor>
    | Tuple of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Union of Ctor<'Ctor> * Expr<'Var, 'Ctor>
    | Throw
    | If of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Match of Expr<'Var, 'Ctor> * Map<Ctor<'Ctor>, 'Var * Expr<'Var, 'Ctor>> * ('Var * Expr<'Var, 'Ctor>) option
    | VarRef of 'Var
    | Not of Expr<'Var, 'Ctor>
    | And of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Or of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Eq of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Plus of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Time of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Minus of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Less of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | TupleFst of Expr<'Var, 'Ctor>
    | TupleSnd of Expr<'Var, 'Ctor>
    | ListEmpty
    | ListCons of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | ListNth of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | ListLen of Expr<'Var, 'Ctor>
    | SetEmpty
    | SetRange of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetInsert of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetMem of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetFilter of 'Var * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | SetExists of 'Var * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | MapEmpty
    | MapAdd of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | MapFindOpt of Expr<'Var, 'Ctor> * Expr<'Var, 'Ctor>
    | Univ of Type<'Ctor>

let rec ofVal (v: Val<'Ctor>) : Expr<'Var, 'Ctor> =
    match v with
    | VUnit -> Lit v
    | VNat _ -> Lit v
    | VBool _ -> Lit v
    | VEvent _ -> Lit v
    | VTuple(l, r) -> Tuple(ofVal l, ofVal r)
    | VSet s -> Set.fold (fun acc v -> SetInsert(ofVal v, acc)) SetEmpty s
    | VList vs -> List.fold (fun acc v -> ListCons(ofVal v, acc)) ListEmpty vs
    | VMap m -> Map.fold (fun acc k v -> MapAdd((ofVal k), (ofVal v), acc)) MapEmpty m
    | VUnion(c, v) -> Union(c, ofVal v)
    | VError -> Throw

let rec range (n1: uint32) (n2: uint32) : Set<Val<'C>> =
    if n1 > n2 then failwith "n1 > n2"
    else if n1 = n2 then Set.empty
    else Set.add (VNat n1) (range (n1 + 1u) n2)

let addAll (m: Map<'K, 'V>) (ps: List<'K * 'V>) : Map<'K, 'V> =
    List.fold (fun acc (k, v) -> Map.add k v acc) m ps

let rangeList (n1: uint) (n2: uint) : uint list =
    if n1 > n2 then
        failwith $"the first arg must not be greater than the second one: range {n1} {n2}"

    let rec loop n1 n2 =
        if n1 = n2 then [] else n1 :: (loop (n1 + 1u) n2) in

    loop n1 n2

let natMax = 5u

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
    | TNat -> List.map VNat (rangeList 0u natMax)
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
                []
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

    | TUnion(_, tm) ->
        List.map VUnion (List.collect (fun (c, t) -> List.map (fun v -> (c, v)) (univ t)) (Map.toList tm))
    | TEvent t -> List.map VEvent (univ t)
    | TError -> [ VError ]

let rec eval (env: Env<'Var, 'Ctor>) (expr: Expr<'Var, 'Ctor>) : Val<'Ctor> =
    match expr with
    | Lit v -> v
    | Tuple(l, r) -> VTuple(eval env l, eval env r)
    | Union(c, e) -> VUnion(c, eval env e)
    | Throw -> VError
    | If(e1, e2, e3) ->
        match eval env e1 with
        | VBool true -> eval env e2
        | VBool false -> eval env e3
        | _ -> VError
    | Match(e, m, d) ->
        match eval env e with
        | VUnion(c, v) ->
            match Map.tryFind c m with
            | Some(x, e1) ->
                if Map.containsKey x env then
                    VError // NOTE: 変数シャドウはとりあえず落とす
                else
                    eval (Map.add x v env) e1
            | None ->
                match d with
                | Some(x, e2) -> eval (Map.add x (VUnion(c, v)) env) e2
                | None -> VError
        | _ -> VError
    | VarRef v -> Map.find v env
    | Not e ->
        match eval env e with
        | VBool b -> VBool(not b)
        | _ -> VError
    | And(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(b1 && b2)
        | _ -> VError
    | Or(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(b1 || b2)
        | _ -> VError
    | Eq(e1, e2) ->
        match eval env e1, eval env e2 with
        | v1, v2 -> VBool(v1 = v2)
    | Less(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(b1 < b2)
        | VNat n1, VNat n2 -> VBool(n1 < n2)
        | VSet s1, VSet s2 -> VBool(Set.isProperSubset s1 s2)
        | _ -> VError
    | Plus(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(b1 || b2)
        | VNat n1, VNat n2 -> VNat(n1 + n2)
        | VSet v1, VSet v2 -> VSet(Set.union v1 v2)
        | _ -> VError
    | Time(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(b1 && b2)
        | VNat n1, VNat n2 -> VNat(n1 * n2)
        | VSet v1, VSet v2 -> VSet(Set.intersect v1 v2)
        | _ -> VError
    | Minus(e1, e2) ->
        match eval env e1, eval env e2 with
        | VBool b1, VBool b2 -> VBool(if b2 then false else b1)
        | VNat n1, VNat n2 -> VNat(if n1 < n2 then 0u else n1 - n2)
        | VSet v1, VSet v2 -> VSet(Set.difference v1 v2)
        | _ -> VError
    | TupleFst e ->
        match eval env e with
        | VTuple(l, _) -> l
        | _ -> VError
    | TupleSnd e ->
        match eval env e with
        | VTuple(_, r) -> r
        | _ -> VError
    | ListEmpty -> VList []
    | ListCons(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, VList vs -> VList(v :: vs)
        | _ -> VError
    | ListNth(e1, e2) ->
        match eval env e1, eval env e2 with
        | VNat n, VList vs -> List.item (Checked.int n) vs
        | _ -> VError
    | ListLen e ->
        match eval env e with
        | VList vs -> VNat(Checked.uint32 (List.length vs))
        | _ -> VError
    | SetEmpty -> VSet Set.empty
    | SetRange(e1, e2) ->
        match eval env e1, eval env e2 with
        | VNat n1, VNat n2 when n1 <= n2 -> VSet(range n1 n2)
        | _ -> VError
    | SetInsert(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, VSet vs -> VSet(Set.add v vs)
        | _ -> VError
    | SetMem(e1, e2) ->
        match eval env e1, eval env e2 with
        | v, VSet vs -> VBool(Set.contains v vs)
        | _ -> VError
    | SetFilter(x, e1, e2) ->
        match eval env e2 with
        | VSet vs ->
            (Set.fold
                (fun acc v ->
                    match acc with
                    | VSet vsAcc ->
                        match eval (Map.add x v env) e1 with
                        | VBool b -> VSet(if b then Set.add v vsAcc else vs)
                        | _ -> VError
                    | _ -> VError)
                (VSet Set.empty)
                vs)
        | _ -> VError
    | SetExists(x, e1, e2) ->
        match eval env e2 with
        | VSet vs ->
            (Set.fold
                (fun acc v ->
                    match acc with
                    | VBool true -> VBool true
                    | VBool false ->
                        match eval (Map.add x v env) e1 with
                        | VBool b -> VBool b
                        | _ -> VError
                    | _ -> VError)
                (VBool false)
                vs)
        | _ -> VError
    | MapEmpty -> VMap Map.empty
    | MapAdd(e1, e2, e3) ->
        match eval env e1, eval env e2, eval env e3 with
        | k, v, VMap m -> VMap(Map.add k v m)
        | _ -> VError
    | MapFindOpt(e1, e2) ->
        match eval env e1, eval env e2 with
        | k, VMap m ->
            match Map.tryFind k m with
            | Some v -> VUnion(CtorSome, v)
            | None -> VUnion(CtorNone, VUnit)
        | _ -> VError
    | Univ t -> VSet(Set.ofList (univ t))

let rec format (expr: Expr<'Var, 'Ctor>) : string =
    match expr with
    | Lit v -> Val.format v
    | Tuple(l, r) -> $"({format l}, {format r})"
    | Union(c, e) ->
        match c with
        | Ctor c' -> if e = Lit VUnit then $"{c'}" else $"({c'} {format e})"
        | CtorSome -> $"(Some {format e})"
        | CtorNone -> "None"
        | CtorLeft -> $"(Left {format e})"
        | CtorRight -> $"(Right {format e})"
    | Throw -> "throw"
    | If(e1, e2, e3) -> $"(if {format e1} then {format e2} else {format e3})"
    | Match(e, cs, d) ->
        let sep = " | " in
        let cs' = List.map (fun (c, (v, e')) -> $"{c} {v} -> {format e'}") (Map.toList cs) in

        match d with
        | Some(v, e') -> $"(match {format e} with {String.concat sep cs'} | {v} -> {format e'})"
        | None -> $"(match {format e} with {String.concat sep cs'})"
    | VarRef v -> $"{v}"
    | Not e -> $"(not ({format e}))"
    | And(e1, e2) -> $"({format e1} && {format e2})"
    | Or(e1, e2) -> $"({format e1} || {format e2})"
    | Eq(e1, e2) -> $"({format e1} = {format e2})"
    | Less(e1, e2) -> $"({format e1} < {format e2})"
    | Plus(e1, e2) -> $"({format e1} + {format e2})"
    | Time(e1, e2) -> $"({format e1} * {format e2})"
    | Minus(e1, e2) -> $"({format e1} - {format e2})"
    | TupleFst e -> $"(fst {format e})"
    | TupleSnd e -> $"(snd {format e})"
    | ListEmpty -> "[]"
    | ListCons(e1, e2) -> $"({format e1} :: {format e2})"
    | ListNth(e1, e2) -> $"(List.nth {format e1} {format e2})"
    | ListLen e -> $"(List.length {format e})"
    | SetEmpty -> "{}"
    | SetRange(e1, e2) -> $"(Set.range {format e1} {format e2})"
    | SetInsert(e1, e2) -> $"(Set.add {format e1} {format e2})"
    | SetMem(e1, e2) -> $"(Set.contains {format e1} {format e2})"
    | SetFilter(v, e1, e2) -> $"(Set.filter (fun {v} -> {format e1}) {format e2})"
    | SetExists(v, e1, e2) -> $"(Set.exists (fun {v} -> {format e1}) {format e2})"
    | MapEmpty -> "Map.empty"
    | MapAdd(k, v, m) -> $"(Map.add {format k} {format v} {format m})"
    | MapFindOpt(e1, e2) -> $"(Map.findOpt {format e1} {format e2})"
    | Univ t -> $"(univ::{Type.format t})"
