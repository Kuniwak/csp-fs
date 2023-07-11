open CSP.Core.Env
open CSP.Core.Val
open CSP.Core.Type
open CSP.Core.Expr
open CSP.Core.Proc
open CSP.Core.Graph
open CSP.Core.ProcMap

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
    | EvLoginBtn
    | EvLogoutBtn
    | EvSearchBtn
    | ChAuthReq
    | ChAuthRes
    | ChSearchReq
    | ChSearchRes
    | ChChkStarReq
    | ChChkStarRes
    | ChStarReq
    | ChStarRes
    | ChUnstarReq
    | ChUnstarRes
    | ChPATField
    | ChSearchField
    | ChChkStarBtn
    | ChStarBtn
    | ChUnstarBtn
    | ChDispLogin
    | ChDispSearch

let tEvent =
    TUnion(
        "tEvent",
        Map[(Ctor EvLoginBtn, TUnit)
            (Ctor EvLogoutBtn, TUnit)
            (Ctor EvSearchBtn, TUnit)]
    )

let tPAT =
    TUnion("pat", Map [ (Ctor PAT1, TUnit); (Ctor PAT2, TUnit); (Ctor PATEmpty, TUnit) ])

let tQuery =
    TUnion("query", Map [ (Ctor Query1, TUnit); (Ctor Query2, TUnit); (Ctor QueryEmpty, TUnit) ])

let tUser = TUnion("user", Map [ (Ctor User1, TUnit) ])

let tRepo =
    TUnion("repo", Map [ (Ctor Repo1, TUnit); (Ctor Repo2, TUnit); (Ctor Repo3, TUnit) ])

let tGHAuthError = TUnion("ghAuthError", Map [ (Ctor GHAuthError, TUnit) ])
let tGHSearchError = TUnion("ghSearchError", Map [ (Ctor GHSearchError, TUnit) ])

let tGHSearchMoreError =
    TUnion("ghSearchMoreError", Map [ (Ctor GHSearchMoreError, TUnit) ])

let tGHChkStarError = TUnion("ghChkStarError", Map [ (Ctor GHChkStarError, TUnit) ])
let tGHStarError = TUnion("ghStarError", Map [ (Ctor GHStarError, TUnit) ])
let tGHUnstarError = TUnion("ghUnstarError", Map [ (Ctor GHUnstarError, TUnit) ])

let tDispLoginError =
    TUnion("tDispLoginError", Map [ (Ctor AppAuthError, TUnit); (Ctor AppAuthFailed, TUnit) ])

let tDispLogin =
    TUnion("either", Map [ (CtorLeft, tDispLoginError); (CtorRight, tOption tUser) ])

let tDispSearchError =
    TUnion(
        "tDispSearchError",
        Map
            [ (Ctor AppSearchError, TUnit)
              (Ctor AppSearchMoreError, TUnit)
              (Ctor AppChkStarError, TUnit)
              (Ctor AppStarError, TUnit)
              (Ctor AppUnstarError, TUnit) ]
    )

let tDispSearch =
    TUnion(
        "either",
        Map
            [ (CtorLeft, tDispSearchError)
              (CtorRight, TTuple(TList tRepo, TTuple(TMap(tRepo, tOption TBool), TBool))) ]
    )

let tChAuthReq = TUnion("tChAuthReq", Map [ (Ctor ChAuthReq, tPAT) ])

let tChAuthRes =
    TUnion("tChAuthRes", Map [ (Ctor ChAuthRes, tEither tGHAuthError (tOption tUser)) ])

let tChSearchReq =
    TUnion("tChSearchReq", Map [ (Ctor ChSearchReq, TTuple(tRepo, TNat)) ])

let tChSearchRes =
    TUnion("tChSearchRes", Map [ (Ctor ChSearchRes, tEither tGHSearchError (TTuple(TList tRepo, TBool))) ])

let tChChkStarReq =
    TUnion("tChChkStarReq", Map [ (Ctor ChChkStarReq, TTuple(tRepo, tPAT)) ])

let tChChkStarRes =
    TUnion("tChChkStarRes", Map [ (Ctor ChChkStarRes, tEither tGHChkStarError TBool) ])

let tChStarReq = TUnion("tChStarReq", Map [ (Ctor ChStarReq, TTuple(tRepo, tPAT)) ])

let tChStarRes =
    TUnion("tChStarRes", Map [ (Ctor ChStarRes, tEither tGHStarError TBool) ])

let tChUnstarReq =
    TUnion("tChUnstarReq", Map [ (Ctor ChUnstarReq, TTuple(tRepo, tPAT)) ])

let tChUnstarRes =
    TUnion("tChUnstarRes", Map [ (Ctor ChUnstarRes, tEither tGHUnstarError TBool) ])

let tChPATField = TUnion("tChPATField", Map [ (Ctor ChPATField, tPAT) ])
let tChSearchField = TUnion("tChSearchField", Map [ (Ctor ChSearchField, tQuery) ])
let tChChkStarBtn = TUnion("tChChkStarBtn", Map [ (Ctor ChChkStarBtn, tRepo) ])
let tChStarBtn = TUnion("tChStarBtn", Map [ (Ctor ChStarBtn, tRepo) ])
let tChUnstarBtn = TUnion("tChUnstarBtn", Map [ (Ctor ChUnstarBtn, tRepo) ])
let tChDispLogin = TUnion("tChDispLogin", Map [ (Ctor ChDispLogin, tDispLogin) ])

let tChDispSearch =
    TUnion("tChDispSearch", Map [ (Ctor ChDispSearch, tDispSearch) ])

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

let m: ProcMap<ProcName, VarName, CtorName> =
    Map
        [ (GHAuth, (None, PrefixRecv(Univ(tChAuthReq), P, Unwind(GHAuthRecv, Some(VarRef P)))))
          (GHAuthRecv, (Some P, IntCh(Unwind(GHAuthWillFail, Some(VarRef P)), Unwind(GHAuthWillResp, Some(VarRef P)))))
          (GHAuthWillFail,
           (Some P,
            Prefix(Union(Ctor ChAuthRes, Union(CtorLeft, Union(Ctor GHAuthError, Lit VUnit))), Unwind(GHAuth, None))))
          (GHAuthWillResp,
           (Some P,
            Prefix(Union(Ctor ChAuthRes, Union(CtorRight, MapFindOpt(VarRef P, VarRef PATRel))), Unwind(GHAuth, None))))

          (GHSearch, (None, PrefixRecv(Univ tChSearchReq, R, Unwind(GHSearchRecv, Some(VarRef R)))))
          (GHSearchRecv, (Some R, IntCh(Unwind(GHSearchWillFail, None), Unwind(GHSearchWillResp, Some(VarRef R)))))
          (GHSearchWillFail,
           (None,
            Prefix(
                Union(Ctor ChSearchRes, Union(CtorLeft, Union(Ctor GHSearchError, Lit VUnit))),
                Unwind(GHSearch, None)
            )))
          (GHSearchWillResp,
           (Some R,
            Prefix(
                Union(
                    Ctor ChSearchRes,
                    Union(
                        CtorRight,
                        Expr.If(
                            Eq(TupleFst(VarRef R), Union(Ctor Query1, Lit VUnit)),
                            // Q1 Hit.
                            Tuple(
                                Expr.If(
                                    // fst r < List.length p1
                                    Less(TupleSnd(VarRef R), Plus(ListLen(VarRef Pages1), Lit(VNat 1u))),
                                    // Requested page.
                                    ListNth(VarRef Pages1, Plus(TupleSnd(VarRef R), Lit(VNat 1u))),
                                    // Out of range.
                                    ListEmpty
                                ),
                                Less(TupleSnd(VarRef R), Minus(ListLen(VarRef Pages1), Lit(VNat 1u)))
                            ),
                            // Others.
                            Tuple(ListEmpty, Lit(VBool false))
                        )
                    )
                ),
                Unwind(GHSearch, None)
            )))
          (GHStar,
           (Some StarRel,
            ExtCh(
                PrefixRecv(Univ(tChChkStarReq), T, Unwind(GHChkStarRecv1, Some(VarRef T))),
                ExtCh(
                    PrefixRecv(Univ(tChStarReq), T, Unwind(GHStarRecv1, Some(VarRef T))),
                    PrefixRecv(Univ(tChUnstarReq), T, Unwind(GHUnstarRecv1, Some(VarRef T)))
                )
            )))
          (GHChkStarRecv1,
           (Some T,
            Match(
                MapFindOpt(TupleSnd(VarRef T), VarRef PATRel),
                Map
                    [ (CtorSome, (U, Unwind(GHChkStarRecv2, Some(Tuple(TupleFst(VarRef T), VarRef U)))))
                      (CtorNone, (Any, Unwind(GHChkStarWillFail, None))) ],
                None
            )))
          (GHChkStarRecv2, (Some T, IntCh(Unwind(GHChkStarWillFail, None), Unwind(GHChkStarWillResp, Some(VarRef T)))))
          (GHChkStarWillFail,
           (None,
            Prefix(
                Union(Ctor ChChkStarRes, Union(CtorLeft, Union(Ctor GHChkStarError, Lit VUnit))),
                Unwind(GHStar, Some(VarRef StarRel))
            )))
          (GHChkStarWillResp,
           (Some T,
            Prefix(Union(Ctor ChChkStarRes, Union(CtorRight, SetMem(VarRef T, VarRef StarRel))), Unwind(GHStar, None))))

          (GHStarRecv1,
           (Some T,
            Match(
                MapFindOpt(TupleSnd(VarRef T), VarRef PATRel),
                Map
                    [ (CtorSome, (U, Unwind(GHStarRecv2, Some(Tuple(TupleFst(VarRef T), VarRef U)))))
                      (CtorNone, (Any, Unwind(GHStarWillFail, None))) ],
                None
            )))
          (GHStarRecv2, (Some T, IntCh(Unwind(GHStarWillFail, None), Unwind(GHStarWillResp, Some(VarRef T)))))
          (GHStarWillFail,
           (None,
            Prefix(
                Union(Ctor ChStarRes, Union(CtorLeft, Union(Ctor GHStarError, Lit VUnit))),
                Unwind(GHStar, Some(VarRef StarRel))
            )))
          (GHStarWillResp,
           (Some T,
            Prefix(
                Union(Ctor ChStarRes, Union(CtorRight, Not(SetMem(VarRef T, VarRef StarRel)))),
                Unwind(GHStar, Some(SetInsert(VarRef T, VarRef StarRel)))
            )))

          (GHUnstarRecv1,
           (Some T,
            Match(
                MapFindOpt(TupleSnd(VarRef T), VarRef PATRel),
                Map
                    [ (CtorSome, (U, Unwind(GHUnstarRecv2, Some(Tuple(TupleFst(VarRef T), VarRef U)))))
                      (CtorNone, (Any, Unwind(GHUnstarWillFail, None))) ],
                None
            )))
          (GHUnstarRecv2, (Some T, IntCh(Unwind(GHUnstarWillFail, None), Unwind(GHUnstarWillResp, Some(VarRef T)))))
          (GHUnstarWillFail,
           (None,
            Prefix(
                Union(Ctor ChStarRes, Union(CtorLeft, Union(Ctor GHUnstarError, Lit VUnit))),
                Unwind(GHStar, Some(VarRef StarRel))
            )))
          (GHUnstarWillResp,
           (Some T,
            Prefix(
                Union(Ctor ChStarRes, Union(CtorRight, SetMem(VarRef T, VarRef StarRel))),
                Unwind(GHStar, Some(SetInsert(VarRef T, VarRef StarRel)))
            )))

          (AppLaunch,
           (Some OptP,
            Match(
                VarRef P,
                Map
                    [ (CtorSome, (P, Unwind(AppDispSearch, Some(VarRef P))))
                      (CtorNone, (Any, Unwind(AppDispLogin, Some(Union(Ctor PATEmpty, Lit VUnit))))) ],
                None
            )))
          (AppDispLogin,
           (Some P1,
            ExtCh(
                PrefixRecv(Univ(tChPATField), P2, Unwind(AppDispLogin, Some(VarRef P2))),
                Guard(
                    Not(Eq(VarRef P1, Union(Ctor PATEmpty, Lit VUnit))),
                    Prefix(Union(Ctor EvLoginBtn, Lit VUnit), Unwind(AppDidPressLoginBtn, Some(VarRef P1)))
                )
            )))
          (AppDidPressLoginBtn, (Some P, Prefix(Union(Ctor ChAuthReq, VarRef P), Unwind(AppReqAuth, Some(VarRef P)))))
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
           (Some P,
            Prefix(Union(Ctor ChDispLogin, Union(Ctor AppAuthError, Lit VUnit)), Unwind(AppDispLogin, Some(VarRef P)))))
          (AppDialogAuthFailed,
           (None,
            Prefix(
                Union(Ctor ChDispLogin, Union(Ctor AppAuthFailed, Lit VUnit)),
                Unwind(AppDispLogin, Some(Union(Ctor PATEmpty, Lit VUnit)))
            )))
          (AppRecvAuth,
           (Some P,
            Prefix(
                Union(Ctor ChDispLogin, Union(CtorRight, Union(CtorNone, Lit VUnit))),
                Unwind(AppDispSearch, Some(Union(Ctor QueryEmpty, Lit VUnit)))
            )))
          (AppDispSearch,
           (Some Q,
            ExtCh(
                Prefix(Union(Ctor EvLogoutBtn, Lit VUnit), Unwind(AppDispLogin, Some(Union(Ctor PATEmpty, Lit VUnit)))),
                ExtCh(
                    PrefixRecv(Univ(tChSearchField), Q, Unwind(AppDispSearch, Some(VarRef Q))),
                    Guard(
                        Not(Eq(VarRef Q, Union(Ctor QueryEmpty, Lit VUnit))),
                        Prefix(Union (Ctor EvSearchBtn, Lit VUnit), Unwind(AppDidPressSearchBtn, Some(VarRef Q)))
                    )
                )
            ))) ]

let env: Env<VarName, CtorName> =
    Map
        [ (PATRel, VMap(Map [ (VUnion(Ctor PAT1, VUnit), VUnion(Ctor User1, VUnit)) ]))
          (Pages1,
           VList
               [ VList [ VUnion(Ctor Repo1, VUnit); VUnion(Ctor Repo2, VUnit) ]
                 VList [ VUnion(Ctor Repo3, VUnit) ] ]) ]

printfn $"%s{dot 100 m env GHSearch None}"
