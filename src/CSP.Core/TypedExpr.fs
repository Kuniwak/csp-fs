module CSP.Core.TypedExpr

open CSP.Core.Ctor
open CSP.Core.LineNum
open CSP.Core.Type
open CSP.Core.TypeCstr
open CSP.Core.Var

type TypedExpr =
    | LitUnit of TypeCstr * LineNum
    | LitTrue of TypeCstr * LineNum
    | LitFalse of TypeCstr * LineNum
    | LitNat of TypeCstr * uint * LineNum
    | LitEmpty of TypeCstr * Type * LineNum
    | LitError of TypeCstr * LineNum
    | Union of TypeCstr * Ctor * TypedExpr * LineNum
    | Throw of TypeCstr * LineNum
    | If of TypeCstr * TypedExpr * TypedExpr * TypedExpr * LineNum
    | Match of TypeCstr * TypedExpr * Map<Ctor, Var * TypedExpr> * (Var option * TypedExpr) option * LineNum
    | VarRef of TypeCstr * Var * LineNum
    | Eq of TypeCstr * Type * TypedExpr * TypedExpr * LineNum
    | Not of TypeCstr * TypedExpr * LineNum
    | Less of TypeCstr * Type * TypedExpr * TypedExpr * LineNum
    | Plus of TypeCstr * Type * TypedExpr * TypedExpr * LineNum
    | Minus of TypeCstr * Type * TypedExpr * TypedExpr * LineNum
    | Times of TypeCstr * Type * TypedExpr * TypedExpr * LineNum
    | Size of TypeCstr * Type * TypedExpr * LineNum
    | Filter of TypeCstr * Type * Var * TypedExpr * TypedExpr * LineNum
    | Exists of TypeCstr * Type * Var * TypedExpr * TypedExpr * LineNum
    | Tuple of TypeCstr * TypedExpr * TypedExpr * LineNum
    | TupleFst of TypeCstr * TypedExpr * LineNum
    | TupleSnd of TypeCstr * TypedExpr * LineNum
    | ListCons of TypeCstr * TypedExpr * TypedExpr * LineNum
    | ListNth of TypeCstr * TypedExpr * TypedExpr * LineNum
    | SetRange of TypeCstr * TypedExpr * TypedExpr * LineNum
    | SetInsert of TypeCstr * TypedExpr * TypedExpr * LineNum
    | SetMem of TypeCstr * TypedExpr * TypedExpr * LineNum
    | MapAdd of TypeCstr * TypedExpr * TypedExpr * TypedExpr * LineNum
    | MapFindOpt of TypeCstr * TypedExpr * TypedExpr * LineNum
    | Univ of TypeCstr * Type * LineNum
    
let rec format (expr: TypedExpr) : string =
    match expr with
    | LitUnit (tc, _) -> $"(()::{TypeCstr.format tc})"
    | LitTrue (tc, _) -> $"(true::{TypeCstr.format tc})"
    | LitFalse (tc, _) -> $"(false::{TypeCstr.format tc})"
    | LitNat(tc, n, _) -> $"(%d{n}::{TypeCstr.format tc})"
    | LitEmpty(tc, t, _) -> $"({Type.format t}.empty::{TypeCstr.format tc})"
    | LitError (tc, _) -> $"(error::{TypeCstr.format tc})"
    | Union(tc, ctor, e, _) ->
        match e with
        | LitUnit _ -> $"({Ctor.format ctor}::{TypeCstr.format tc})"
        | _ -> $"(({Ctor.format ctor} {format e})::{TypeCstr.format tc})"
    | Throw(tc, _) -> $"(throw::{TypeCstr.format tc})"
    | If(tc, e1, e2, e3, _) -> $"((if {format e1} then {format e2} else {format e3})::{TypeCstr.format tc})"
    | Match(tc, e, cs, d, _) ->
        let sep = " | " in
        let cs' = List.map (fun (c, (v, e')) -> $"{c} {v} -> {format e'}") (Map.toList cs) in

        match d with
        | Some(v, e') -> $"((match {format e} with {String.concat sep cs'} | {v} -> {format e'})::{TypeCstr.format tc})"
        | None -> $"((match {format e} with {String.concat sep cs'})::{TypeCstr.format tc})"
    | VarRef(tc, v, _) -> $"({v}::{TypeCstr.format tc})"
    | Not(tc, expr, _) -> $"((not {format expr})::{TypeCstr.format tc})"
    | Eq(tc, t, expr1, expr2, _) -> $"(({Type.format t}.equal {format expr1} {format expr2})::{TypeCstr.format tc})"
    | Less(tc, t, expr1, expr2, _) -> $"(({Type.format t}.less {format expr1} {format expr2})::{TypeCstr.format tc})"
    | Plus(tc, t, expr1, expr2, _) -> $"(({Type.format t}.plus {format expr1} {format expr2})::{TypeCstr.format tc})"
    | Minus(tc, t, expr1, expr2, _) -> $"(({Type.format t}.minus {format expr1} {format expr2})::{TypeCstr.format tc})"
    | Times(tc, t, expr1, expr2, _) -> $"(({Type.format t}.times {format expr1} {format expr2})::{TypeCstr.format tc})"
    | Size(tc, t, expr, _) -> $"(({Type.format t}.count {format expr})::{TypeCstr.format tc})"
    | Exists(tc, t, var, expr1, expr2, _) -> $"(({Type.format t}.exists (fun {var} -> {format expr1}) {format expr2})::{TypeCstr.format tc})"
    | Filter(tc, t, var, expr1, expr2, _) -> $"(({Type.format t}.filter (fun {var} -> {format expr1}) {format expr2})::{TypeCstr.format tc})"
    | Tuple(tc, l, r, _) -> $"(({format l}, {format r})::{TypeCstr.format tc})"
    | TupleFst(tc, e, _) -> $"((fst {format e})::{TypeCstr.format tc})"
    | TupleSnd(tc, e, _) -> $"((snd {format e})::{TypeCstr.format tc})"
    | ListCons(tc, e1, e2, _) -> $"(({format e1} :: {format e2})::{TypeCstr.format tc})"
    | ListNth(tc, e1, e2, _) -> $"((List.nth {format e1} {format e2})::{TypeCstr.format tc})"
    | SetRange(tc, e1, e2, _) -> $"((Set.range {format e1} {format e2})::{TypeCstr.format tc})"
    | SetInsert(tc, e1, e2, _) -> $"((Set.add {format e1} {format e2})::{TypeCstr.format tc})"
    | SetMem(tc, e1, e2, _) -> $"((Set.contains {format e1} {format e2})::{TypeCstr.format tc})"
    | MapAdd(tc, k, v, m, _) -> $"((Map.add {format k} {format v} {format m})::{TypeCstr.format tc})"
    | MapFindOpt(tc, e1, e2, _) -> $"((Map.findOpt {format e1} {format e2})::{TypeCstr.format tc})"
    | Univ(tc, t, _) -> $"((univ::{Type.format t})::{TypeCstr.format tc})"
    
let line (expr: TypedExpr) : LineNum =
    match expr with
    | LitUnit (_, line) -> line
    | LitTrue (_, line) -> line
    | LitFalse (_, line) -> line
    | LitNat(_, _, line) -> line
    | LitError (_, line) -> line
    | LitEmpty(_, _, line) -> line
    | Union(_, _, _, line) -> line
    | Throw (_, line) -> line
    | If(_, _, _, _, line) -> line
    | Match(_, _, _, _, line) -> line
    | VarRef(_, _, line) -> line
    | Not(_, _, line) -> line
    | Plus(_, _, _, _, line) -> line
    | Times(_, _, _, _, line) -> line
    | Minus(_, _, _, _, line) -> line
    | Less(_, _, _, _, line) -> line
    | Eq(_, _, _, _, line) -> line
    | Size(_, _, _, line) -> line
    | Filter(_, _, _, _, _, line) -> line
    | Exists(_, _, _, _, _, line) -> line
    | Tuple(_, _, _, line) -> line
    | TupleFst(_, _, line) -> line
    | TupleSnd(_, _, line) -> line
    | ListCons(_, _, _, line) -> line
    | ListNth(_, _, _, line) -> line
    | SetRange(_, _, _, line) -> line
    | SetInsert(_, _, _, line) -> line
    | SetMem(_, _, _, line) -> line
    | MapAdd(_, _, _, _, line) -> line
    | MapFindOpt(_, _, _, line) -> line
    | Univ(_, _, line) -> line
