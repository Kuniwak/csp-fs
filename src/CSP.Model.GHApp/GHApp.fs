module CSP.Model.GHApp

open CSP.Core
open CSP.Core.Env
open CSP.Core.Ctor
open CSP.Core.Val
open CSP.Core.Type
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.ProcMap

let tEvent = TUnion("tEvent", TUnit)
let tPAT = TUnion("pat", TUnit)
let tQuery = TUnion("query", TUnit)
let tUser = TUnion("user", TUnit)

let tRepo = TUnion("repo", TUnit)

let tGHAuthError = TUnion("ghAuthError", TUnit)
let tGHSearchError = TUnion("ghSearchError", TUnit)

let tGHSearchMoreError = TUnion("ghSearchMoreError", TUnit)

let tGHChkStarError = TUnion("ghChkStarError", TUnit)
let tGHStarError = TUnion("ghStarError", TUnit)
let tGHUnstarError = TUnion("ghUnstarError", TUnit)

let tDispLoginError = TUnion("dispLoginError", TUnit)

let tDispLogin = TUnion("either", Map [])

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
    TUnion("tChSearchReq", Map [ (Ctor ChSearchReq, TTuple(tQuery, TNat)) ])

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
let cm =
    CtorMap.from
        [ ("event", Ctor "EvLoginBtn", TUnit)
          ("event", Ctor "EvLogoutBtn", TUnit)
          ("event", Ctor "EvSearchBtn", TUnit)
          ("pat", Ctor "Pat1", TUnit)
          ("pat", Ctor "Pat2", TUnit)
          ("pat", Ctor "PatEmpty", TUnit)
          ("query", Ctor "Query1", TUnit)
          ("query", Ctor "Query2", TUnit)
          ("query", Ctor "QueryEmpty", TUnit)
          ("user", Ctor "User1", TUnit)
          ("repo", Ctor "Repo1", TUnit)
          ("repo", Ctor "Repo2", TUnit)
          ("repo", Ctor "Repo3", TUnit)
          ("ghAuthError", Ctor "GHAuthError", TUnit)
          ("ghSearchError", Ctor "GHSearchError", TUnit)
          ("ghSearchMoreError", Ctor "GHSearchMoreError", TUnit)
          ("ghChkStarError", Ctor "GHChkStarError", TUnit)
          ("ghStarError", Ctor "GHStarError", TUnit)
          ("ghUnstarError", Ctor "GHUnstarError", TUnit)
          ("appAuthError", Ctor "AppAuthError", TUnit)
          ("appAuthFailed", Ctor "AppAuthFailed", TUnit)
          (Ctor "Left", tDispLoginError)
          (Ctor "Right", tOption tUser) 
           ]

let model =
    ProcMap
        [ (("GHAuth", None), prefixRecv (Univ tChAuthReq) "p" (unwind GHAuthRecv (Some(varRef "p"))))
          (("GHAuthRecv", Some "p"),
           intCh (unwind "GHAuthWillFail" (Some(varRef "p"))) (unwind "GHAuthWillResp" (Some(varRef "p"))))
          (("GHAuthWillFail", Some "p"),
           prefix (unionExpr "ChAuthRes" (union "Left" (union "GHAuthError" LitUnit))) (unwind "GHAuth" None))
          (("GHAuthWillResp", Some "p"),
           (prefix
               (union "ChAuthRes" (union "Right" (mapFindOpt (varRef "p", varRef "PAT_REL"))))
               (Unwind "GHAuth" None)))

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
                                    NatLess(TupleSnd(VarRef R), NatAdd(ListLen(VarRef Pages1), Lit(VNat 1u))),
                                    // Requested page.
                                    ListNth(VarRef Pages1, NatAdd(TupleSnd(VarRef R), Lit(VNat 1u))),
                                    // Out of range.
                                    ListEmpty
                                ),
                                NatLess(TupleSnd(VarRef R), NatSub(ListLen(VarRef Pages1), Lit(VNat 1u)))
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
                Union(Ctor ChStarRes, Union(CtorRight, BoolNot(SetMem(VarRef T, VarRef StarRel)))),
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
                    BoolNot(Eq(VarRef P1, Union(Ctor PATEmpty, Lit VUnit))),
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
                        BoolNot(Eq(VarRef Q, Union(Ctor QueryEmpty, Lit VUnit))),
                        Prefix(Union(Ctor EvSearchBtn, Lit VUnit), Unwind(AppDidPressSearchBtn, Some(VarRef Q)))
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
