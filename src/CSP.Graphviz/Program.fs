open CSP.Core.Env
open CSP.Core.Val
open CSP.Core.Type
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.Graph
open CSP.Core.ProcMap

type EvName =
    | LoginBtn
    | LogoutBtn
    | SearchBtn

type ChName =
    | AuthReq
    | AuthRes
    | SearchReq
    | SearchRes
    | ChkStarReq
    | ChkStarRes
    | DispLogin
    | DispSearch
    | StarReq
    | StarRes
    | UnstarReq
    | UnstarRes
    | PATField
    | SearchField
    | ChkStarBtn
    | StarBtn
    | UnstarBtn
    
type CtorName =
    | GHAuthError
    | GHSearchError
    | GHSearchMoreError
    | GHChkStarError
    | GHStarError
    | GHUnstarError
    | AppAuthError
    | AppAuthFailed
    | AppSearchError
    | AppSearchMoreError
    | AppChkStarError
    | AppStarError
    | AppUnstarError
    | AppSearch
    | PAT1
    | PAT2
    | PATEmpty
    | User1
    | Query1
    | Query2
    | QueryEmpty
    | Repo1
    | Repo2
    | Repo3
    
let tPAT = TUnion("pat", Map [(Ctor PAT1, TUnit); (Ctor PAT2, TUnit); (Ctor PATEmpty, TUnit)])
let tQuery = TUnion("query", Map [(Ctor Query1, TUnit); (Ctor Query2, TUnit); (Ctor QueryEmpty, TUnit)])
let tUser = TUnion("user", Map [(Ctor User1, TUnit)])
let tRepo = TUnion("repo", Map [(Ctor Repo1, TUnit); (Ctor Repo2, TUnit); (Ctor Repo3, TUnit)])
let tGHAuthError = TUnion("ghAuthError", Map [(Ctor GHAuthError, TUnit)])
let tGHSearchError = TUnion("ghSearchError", Map [(Ctor GHSearchError, TUnit)])
let tGHSearchMoreError = TUnion("ghSearchMoreError", Map [(Ctor GHSearchMoreError, TUnit)])
let tGHChkStarError = TUnion("ghChkStarError", Map [(Ctor GHChkStarError, TUnit)])
let tGHStarError = TUnion("ghStarError", Map [(Ctor GHStarError, TUnit)])
let tGHUnstarError = TUnion("ghUnstarError", Map [(Ctor GHUnstarError, TUnit)])

type VarName =
    | OptP
    | P
    | P1
    | P2
    | Q
    | R
    | T
    | U
    | B
    | Any
    | Pages1
    | PATRel
    | StarRel

type ProcName =
    | GHAuth
    | GHAuthRecv
    | GHAuthWillResp
    | GHAuthWillFail
    | GHSearch
    | GHSearchRecv
    | GHSearchWillResp
    | GHSearchWillFail
    | GHStar
    | GHChkStarRecv1
    | GHChkStarRecv2
    | GHChkStarWillResp
    | GHChkStarWillFail
    | GHStarRecv1
    | GHStarRecv2
    | GHStarWillResp
    | GHStarWillFail
    | GHUnstarRecv1
    | GHUnstarRecv2
    | GHUnstarWillResp
    | GHUnstarWillFail
    | AppLaunch
    | AppDispLogin
    | AppDidPressLoginBtn
    | AppReqAuth
    | AppDialogAuthError
    | AppDialogAuthFailed
    | AppRecvAuth
    | AppDispSearch
    | AppDidPressSearchBtn
    | AppDialogSearchError
    | AppReqSearch
    | AppRecvSearch
    | AppDispSearchResult
    | AppDidPressMoreBtn
    | AppRecvMore
    | AppDialogSearchMoreError
    | AppReqMore
    | AppDidPressChkStar
    | AppReqChkStar
    | AppRecvChkStar
    | AppDialogChKStarError
    | AppDidPressStar
    | AppReqStar
    | AppDialogStarError
    | AppRecvStar
    | AppDidPressUnstar
    | AppReqUnstar
    | AppDialogUnstarError
    | AppRecvUnstar

let m: ProcMap<ProcName, EvName, ChName, VarName, CtorName> =
    Map
        [ (GHAuth, (None, PrefixRecv(AuthReq, P, tPAT, Unwind(GHAuthRecv, Some(VarRef P)))))
          (GHAuthRecv, (Some P, IntCh(Unwind(GHAuthWillFail, Some(VarRef P)), Unwind(GHAuthWillResp, Some(VarRef P)))))
          (GHAuthWillFail,
           (Some P, PrefixSend(AuthRes, LitUnion(CtorLeft, LitUnion(Ctor GHAuthError, LitUnit)), Unwind(GHAuth, None))))
          (GHAuthWillResp,
           (Some P, PrefixSend(AuthRes, LitUnion(CtorRight, MapFindOpt(VarRef P, VarRef PATRel)), Unwind(GHAuth, None))))

          (GHSearch, (None, PrefixRecv(SearchReq, R, TTuple(tQuery, TNat), Unwind(GHSearchRecv, Some(VarRef R)))))
          (GHSearchRecv, (Some R, IntCh(Unwind(GHSearchWillFail, None), Unwind(GHSearchWillResp, Some(VarRef P)))))
          (GHAuthWillFail,
           (None,
            PrefixSend(SearchRes, LitUnion(CtorLeft, LitUnion(Ctor GHSearchError, LitUnit)), Unwind(GHSearch, None))))
          (GHAuthWillResp,
           (Some P,
            PrefixSend(
                SearchRes,
                LitUnion(
                    CtorRight,
                    Expr.If(
                        Eq(TupleFst(VarRef R), LitUnion(Ctor Query1, LitUnit)),
                        // Q1 Hit.
                        LitTuple(
                            Expr.If(
                                // fst r < List.length p1
                                Less(TupleSnd(VarRef R), Plus(ListLen(VarRef Pages1), LitNat 1u)),
                                // Requested page.
                                ListNth(VarRef Pages1, Plus(TupleSnd(VarRef R), LitNat 1u)),
                                // Out of range.
                                ListEmpty
                            ),
                            Less(TupleSnd(VarRef R), Minus(ListLen(VarRef Pages1), LitNat 1u))
                        ),
                        // Others.
                        LitTuple(ListEmpty, LitBool false)
                    )
                ),
                Unwind(GHSearch, None)
            )))
          (GHStar,
           (Some StarRel,
            ExtCh(
                PrefixRecv(ChkStarReq, T, TTuple(tPAT, tRepo), Unwind(GHChkStarRecv1, Some(VarRef T))),
                ExtCh(
                    PrefixRecv(StarReq, T, TTuple(tPAT, tRepo), Unwind(GHStarRecv1, Some(VarRef T))),
                    PrefixRecv(UnstarReq, T, TTuple(tPAT, tRepo), Unwind(GHUnstarRecv1, Some(VarRef T)))
                )
            )))
          (GHChkStarRecv1,
           (Some T,
            Match(
                MapFindOpt(TupleSnd(VarRef T), VarRef PATRel),
                Map
                    [ (CtorSome, (U, Unwind(GHChkStarRecv2, Some(LitTuple(TupleFst(VarRef T), VarRef U)))))
                      (CtorNone, (Any, Unwind(GHChkStarWillFail, None))) ],
                None
            )))
          (GHChkStarRecv2, (Some T, IntCh(Unwind(GHChkStarWillFail, None), Unwind(GHChkStarWillResp, Some(VarRef T)))))
          (GHChkStarWillFail,
           (None,
            PrefixSend(
                ChkStarRes,
                LitUnion(CtorLeft, LitUnion(Ctor GHChkStarError, LitUnit)),
                Unwind(GHStar, Some(VarRef StarRel))
            )))
          (GHChkStarWillResp,
           (Some T, PrefixSend(ChkStarRes, LitUnion(CtorRight, SetMem(VarRef T, VarRef StarRel)), Unwind(GHStar, None))))

          (GHStarRecv1,
           (Some T,
            Match(
                MapFindOpt(TupleSnd(VarRef T), VarRef PATRel),
                Map
                    [ (CtorSome, (U, Unwind(GHStarRecv2, Some(LitTuple(TupleFst(VarRef T), VarRef U)))))
                      (CtorNone, (Any, Unwind(GHStarWillFail, None))) ],
                None
            )))
          (GHStarRecv2, (Some T, IntCh(Unwind(GHStarWillFail, None), Unwind(GHStarWillResp, Some(VarRef T)))))
          (GHStarWillFail,
           (None,
            PrefixSend(
                StarRes,
                LitUnion(CtorLeft, LitUnion(Ctor GHStarError, LitUnit)),
                Unwind(GHStar, Some(VarRef StarRel))
            )))
          (GHStarWillResp,
           (Some T,
            PrefixSend(
                StarRes,
                LitUnion(CtorRight, Not(SetMem(VarRef T, VarRef StarRel))),
                Unwind(GHStar, Some(SetInsert(VarRef T, VarRef StarRel)))
            )))

          (GHUnstarRecv1,
           (Some T,
            Match(
                MapFindOpt(TupleSnd(VarRef T), VarRef PATRel),
                Map
                    [ (CtorSome, (U, Unwind(GHUnstarRecv2, Some(LitTuple(TupleFst(VarRef T), VarRef U)))))
                      (CtorNone, (Any, Unwind(GHUnstarWillFail, None))) ],
                None
            )))
          (GHUnstarRecv2, (Some T, IntCh(Unwind(GHUnstarWillFail, None), Unwind(GHUnstarWillResp, Some(VarRef T)))))
          (GHUnstarWillFail,
           (None,
            PrefixSend(
                StarRes,
                LitUnion(CtorLeft, LitUnion(Ctor GHUnstarError, LitUnit)),
                Unwind(GHStar, Some(VarRef StarRel))
            )))
          (GHUnstarWillResp,
           (Some T,
            PrefixSend(
                StarRes,
                LitUnion(CtorRight, SetMem(VarRef T, VarRef StarRel)),
                Unwind(GHStar, Some(SetInsert(VarRef T, VarRef StarRel)))
            )))

          (AppLaunch,
           (Some OptP,
            Match(
                VarRef P,
                Map
                    [ (CtorSome, (P, Unwind(AppDispSearch, Some(VarRef P))))
                      (CtorNone, (Any, Unwind(AppDispLogin, Some(LitUnion(Ctor PATEmpty, LitUnit))))) ],
                None
            )))
          (AppDispLogin,
           (Some P1,
            ExtCh(
                PrefixRecv(PATField, P2, Unwind(AppDispLogin, Some(VarRef P2))),
                Guard(
                    Not(Eq(VarRef P1, LitUnion(Ctor PATEmpty, LitUnit))),
                    Prefix(LoginBtn, Unwind(AppDidPressLoginBtn, Some(VarRef P1)))
                )
            )))
          (AppDidPressLoginBtn, (Some P, PrefixSend(AuthReq, VarRef P, Unwind(AppReqAuth, Some(VarRef P)))))
          (AppReqAuth,
           (Some P,
            Match(
                VarRef P,
                Map
                    [ (CtorLeft, (Any, Unwind(AppDialogAuthError, Some(VarRef P))))
                      (CtorRight,
                       (B, If(VarRef B, Unwind(AppRecvAuth, Some(VarRef P)), Unwind(AppDialogAuthFailed, None)))) ],
                None
            )))
          (AppDialogAuthError,
           (Some P, PrefixSend(DispLogin, LitUnion(Ctor AppAuthError, LitUnit), Unwind(AppDispLogin, Some(VarRef P)))))
          (AppDialogAuthFailed,
           (None,
            PrefixSend(
                DispLogin,
                LitUnion(Ctor AppAuthFailed, LitUnit),
                Unwind(AppDispLogin, Some(LitUnion(Ctor PATEmpty, LitUnit)))
            )))
          (AppRecvAuth,
           (Some P,
            PrefixSend(
                DispLogin,
                LitUnion(Ctor AppSearch, LitUnit),
                Unwind(AppDispSearch, Some(LitUnion(Ctor QueryEmpty, LitUnit)))
            )))
          (AppDispSearch,
           (Some Q,
            ExtCh(
                Prefix(LoginBtn, Unwind(AppDispLogin, Some(LitUnion(Ctor PATEmpty, LitUnit)))),
                ExtCh(
                    PrefixRecv(SearchField, Q, Unwind(AppDispSearch, Some(VarRef Q))),
                    Guard(
                        Not(Eq(VarRef Q, LitUnion(Ctor QueryEmpty, LitUnit))),
                        Prefix(SearchBtn, Unwind(AppDidPressSearchBtn, Some(VarRef Q)))
                    )
                )
            ))) ]

let env: Env<VarName, CtorName> =
    Map
        [ (PATRel, VMap(Map [ (VUnion(Ctor PAT1, VUnit), VUnion(Ctor User1, VUnit)) ]))
          (Pages1,
           VList
               [ VList [ VUnion(Ctor Repo, VNat 0u); VUnion(Ctor Repo, VNat 1u) ]
                 VList [ VUnion(Ctor Repo, VNat 2u) ] ]) ]

printf "%s" (dot 100 m env GHStar (Some(VSet(Set []))))
