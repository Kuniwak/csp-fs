module CSP.Model.GHApp

open CSP.Core
open CSP.Core.Env
open CSP.Core.Ctor
open CSP.Core.Val
open CSP.Core.Type
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.ProcMap

let tEvent =
    tUnion "event" [ ("EvLoginBtn", []); ("EvLogoutBtn", []); ("EvSearchBtn", []) ]

let tPAT = tUnion "pat" [ ("Pat1", []); ("Pat2", []); ("PatEmpty", []) ]
let tQuery = tUnion "query" [ ("Query1", []); ("Query2", []); ("QueryEmpty", []) ]
let tUser = tUnion "user" [ ("User1", []) ]
let tRepo = tUnion "repo" [ ("Repo1", []); ("Repo2", []); ("Repo3", []) ]

let tGHAuthError = tUnion "ghAuthError" [ ("GHAuthError", []) ]
let tGHSearchError = tUnion "ghSearchError" [ ("GHSearchError", []) ]

let tGHSearchMoreError = tUnion "ghSearchMoreError" [ ("GHSearchMoreError", []) ]

let tGHChkStarError = tUnion "ghChkStarError" [ ("GHChkStarError", []) ]
let tGHStarError = tUnion "ghStarError" [ ("GHStarError", []) ]
let tGHUnstarError = tUnion "ghUnstarError" [ ("GHUnstarError", []) ]

let tDispLoginError = tUnion "dispLoginError" [ ("DispLoginError", []) ]

let tDispLogin = tUnion "either" [ ("Left", [ TVar 0u ]); ("Right", [ TVar 1u ]) ]

let tDispSearchError =
    tUnion
        "tDispSearchError"
        [ ("AppSearchError", [])
          ("AppSearchMoreError", [])
          ("AppChkStarError", [])
          ("AppStarError", [])
          ("AppUnstarError", []) ]

let tDispSearch =
    tEither tDispSearchError (tTuple [ tList tRepo; tMap tRepo (tOption tBool); TBool ])

let tChAuthReq = tUnion "tChAuthReq" [ ("ChAuthReq", [ tPAT ]) ]

let tChAuthRes =
    tUnion "tChAuthRes" [ ("ChAuthRes", [ tEither tGHAuthError (tOption tUser) ]) ]

let tChSearchReq =
    tUnion "tChSearchReq" [ ("ChSearchReq", [ tTuple2 tQuery tNat ]) ]

let tChSearchRes =
    tUnion "tChSearchRes" [ ("ChSearchRes", [ tEither tGHSearchError (tTuple2 (TList tRepo) TBool) ]) ]

let tChChkStarReq =
    tUnion "tChChkStarReq" [ ("ChChkStarReq", [ tTuple2 tRepo tPAT ]) ]

let tChChkStarRes =
    tUnion "tChChkStarRes" [ ("ChChkStarRes", [ tEither tGHChkStarError tBool ]) ]

let tChStarReq = tUnion "tChStarReq" [ ("ChStarReq", [ tTuple2 tRepo tPAT ]) ]

let tChStarRes =
    tUnion "tChStarRes" [ ("ChStarRes", [ tEither tGHStarError TBool ]) ]

let tChUnstarReq = tUnion "tChUnstarReq" [ ("ChUnstarReq", [ tTuple2 tRepo tPAT ]) ]

let tChUnstarRes =
    tUnion "tChUnstarRes" [ ("ChUnstarRes", [ tEither tGHUnstarError tBool ]) ]

let tChPATField = tUnion "tChPATField" [ ("ChPATField", [ tPAT ]) ]
let tChSearchField = tUnion "tChSearchField" [ ("ChSearchField", [ tQuery ]) ]
let tChChkStarBtn = tUnion "tChChkStarBtn" [ ("ChChkStarBtn", [ tRepo ]) ]
let tChStarBtn = tUnion "tChStarBtn" [ ("ChStarBtn", [ tRepo ]) ]
let tChUnstarBtn = tUnion "tChUnstarBtn" [ ("ChUnstarBtn", [ tRepo ]) ]
let tChDispLogin = tUnion "tChDispLogin" [ ("ChDispLogin", [ tDispLogin ]) ]

let tChDispSearch = tUnion "tChDispSearch" [ ("ChDispSearch", [ tDispSearch ]) ]

let cm =
    CtorMap.from
        [
          tEvent
          tPAT
          tQuery
          tUser
          tRepo
          tGHAuthError
          tGHSearchError
          tGHSearchMoreError
          tGHChkStarError
          tGHStarError
          tGHUnstarError
          tDispLoginError
          tDispLogin
          tDispSearchError
          tDispSearch
          tChAuthReq
          tChAuthRes
          tChSearchReq
          tChSearchRes
          tChChkStarReq
          tChChkStarRes
          tChStarReq
          tChStarRes
          tChUnstarReq
          tChUnstarRes
          tChPATField
          tChSearchField
          tChChkStarBtn
          tChStarBtn
          tChUnstarBtn
          tChDispLogin
          tChDispSearch ]

let model =
    from
        [ (("GHAuth", None), prefixRecv (univ tChAuthReq) "p" (unwind "GHAuthRecv" (Some(varRef "p"))))
          (("GHAuthRecv", Some "p"), intCh (unwind "GHAuthWillFail" (Some(varRef "p"))) (unwind "GHAuthWillResp" (Some(varRef "p"))))
          (("GHAuthWillFail", Some "p"), prefix (ctor "ChAuthRes" [(ctor "Left" [(ctor "GHAuthError" [litUnit])])]) (unwind "GHAuth" None))
          (("GHAuthWillResp", Some "p"), (prefix (ctor "ChAuthRes" [(ctor "Right" [(mapFindOpt (varRef "p", varRef "PAT_REL"))])]) (unwind "GHAuth" None)))

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
