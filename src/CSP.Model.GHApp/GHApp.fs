module CSP.Model.GHApp

open CSP.Core
open CSP.Core.Env
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.Util
open CSP.Core.ValShorthand
open CSP.Core.ProcMap

let tEvent =
    tUnion
        "event"
        [ ("EvLoginBtn", [])
          ("EvLogoutBtn", [])
          ("EvSearchBtn", [])
          ("EvMoreBtn", []) ]

let tPat = tUnion "pat" [ ("Pat1", []); ("Pat2", []); ("PatEmpty", []) ]

let tQuery = tUnion "query" [ ("Query1", []); ("Query2", []); ("QueryEmpty", []) ]

let tUser = tUnion "user" [ ("User1", []) ]
let tRepo = tUnion "repo" [ ("Repo1", []); ("Repo2", []); ("Repo3", []) ]

let tGHAuthError = tUnion "ghAuthError" [ ("GHAuthError", []) ]
let tGHSearchError = tUnion "ghSearchError" [ ("GHSearchError", []) ]

let tGHSearchMoreError = tUnion "ghSearchMoreError" [ ("GHSearchMoreError", []) ]

let tGHChkStarError = tUnion "ghChkStarError" [ ("GHChkStarError", []) ]
let tGHStarError = tUnion "ghStarError" [ ("GHStarError", []) ]
let tGHUnstarError = tUnion "ghUnstarError" [ ("GHUnstarError", []) ]

let tDispLogin =
    tUnion "tDispLogin" [ ("AppAuthSuccess", []); ("AppAuthError", []); ("AppAuthFailed", []) ]

let tDispSearchError =
    tUnion
        "tDispSearchError"
        [ ("AppSearchError", [])
          ("AppSearchMoreError", [])
          ("AppChkStarError", [])
          ("AppStarError", [])
          ("AppUnstarError", []) ]



let tChAuthReq = tUnion "tChAuthReq" [ ("ChAuthReq", [ tPat ]) ]

let tChAuthRes =
    tUnion "tChAuthRes" [ ("ChAuthResError", [ tGHAuthError ]); ("ChAuthResOk", [ tOption tUser ]) ]

let tChSearchReq = tUnion "tChSearchReq" [ ("ChSearchReq", [ tQuery; tNat ]) ]

let tChSearchRes =
    tUnion
        "tChSearchRes"
        [ ("ChSearchResError", [ tGHSearchError ])
          ("ChSearchResOk", [ (tList tRepo); tBool ]) ]


let tChChkStarReq = tUnion "tChChkStarReq" [ ("ChChkStarReq", [ tRepo; tPat ]) ]

let tChChkStarRes =
    tUnion "tChChkStarRes" [ ("ChChkStarRes", [ tEither tGHChkStarError tBool ]) ]

let tChStarReq = tUnion "tChStarReq" [ ("ChStarReq", [ tRepo; tPat ]) ]

let tChStarRes =
    tUnion "tChStarRes" [ ("ChStarRes", [ tEither tGHStarError tBool ]) ]

let tChUnstarReq = tUnion "tChUnstarReq" [ ("ChUnstarReq", [ tRepo; tPat ]) ]

let tChUnstarRes =
    tUnion "tChUnstarRes" [ ("ChUnstarRes", [ tEither tGHUnstarError tBool ]) ]

let tChPatField = tUnion "tChPATField" [ ("ChPATField", [ tPat ]) ]

let tChSearchField = tUnion "tChSearchField" [ ("ChSearchField", [ tQuery ]) ]

let tChChkStarBtn = tUnion "tChChkStarBtn" [ ("ChChkStarBtn", [ tRepo ]) ]
let tChStarBtn = tUnion "tChStarBtn" [ ("ChStarBtn", [ tRepo ]) ]
let tChUnstarBtn = tUnion "tChUnstarBtn" [ ("ChUnstarBtn", [ tRepo ]) ]

let tChDispLogin = tUnion "tChDispLogin" [ ("ChDispLogin", [ tDispLogin ]) ]

let tChDispSearch =
    tUnion
        "tChDispSearch"
        [ ("ChDispSearchError", [])
          ("ChDispSearchOk", [ tList tRepo; tMap tRepo (tOption tBool); tBool ]) ]

let tPage = tList tRepo
let tPages = tList tPage
let tStarRel = tSet (tTuple2 tRepo tUser)

let ctorMap =
    ResultEx.get
        CtorMapError.format
        (CtorMap.from
            [ tOption (tVar 0u)
              tEither (tVar 0u) (tVar 1u)
              tEvent
              tPat
              tQuery
              tUser
              tRepo
              tGHAuthError
              tGHSearchError
              tGHSearchMoreError
              tGHChkStarError
              tGHStarError
              tGHUnstarError
              tDispLogin
              tDispSearchError
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
              tChPatField
              tChSearchField
              tChChkStarBtn
              tChStarBtn
              tChUnstarBtn
              tChDispLogin
              tChDispSearch ])

let procMap =
    from
        [ (("GHAuth", []),
           prefixRecv
               (univ tChAuthReq __LINE__)
               "ch"
               (``match``
                   (varRef "ch" __LINE__)
                   [ (("ChAuthReq", [ "p" ]), (unwind "GHAuthRecv" [ varRef "p" __LINE__ ] __LINE__)) ]
                   __LINE__)
               __LINE__)

          (("GHAuthRecv", [ ("p", tPat) ]),
           intCh
               (unwind "GHAuthWillFail" [ varRef "p" __LINE__ ] __LINE__)
               (unwind "GHAuthWillResp" [ varRef "p" __LINE__ ] __LINE__)
               __LINE__)

          (("GHAuthWillFail", [ ("p", tPat) ]),
           prefix
               (ctor "ChAuthRes" [ (ctor "Left" [ ctor "GHAuthError" [] __LINE__ ] __LINE__) ] __LINE__)
               (unwind "GHAuth" [] __LINE__)
               __LINE__)

          (("GHAuthWillResp", [ ("p", tPat) ]),
           (prefix
               (ctor
                   "ChAuthRes"
                   [ (ctor "Right" [ mapFindOpt (varRef "p" __LINE__) (varRef "PAT_REL" __LINE__) __LINE__ ] __LINE__) ]
                   __LINE__)
               (unwind "GHAuth" [] __LINE__))
               __LINE__)

          (("GHSearch", []),
           prefixRecv
               (univ tChSearchReq __LINE__)
               "r"
               (``match``
                   (varRef "r" __LINE__)
                   [ (("ChSearchReq", [ "q"; "i" ]),
                      unwind "GHSearchRecv" [ (varRef "q" __LINE__); (varRef "i" __LINE__) ] __LINE__) ]
                   __LINE__)
               __LINE__)

          (("GHSearchRecv", [ ("q", tQuery); ("i", tNat) ]),
           intCh
               (unwind "GHSearchWillFail" [] __LINE__)
               (unwind "GHSearchWillResp" [ varRef "q" __LINE__; varRef "i" __LINE__ ] __LINE__)
               __LINE__)

          (("GHSearchWillFail", []),
           prefix
               (ctor "ChSearchRes" [ ctor "Left" [ ctor "GHSearchError" [] __LINE__ ] __LINE__ ] __LINE__)
               (unwind "GHSearch" [] __LINE__)
               __LINE__)

          (("GHSearchWillResp", [ ("q", tQuery); ("i", tNat) ]),
           prefix
               (ctor
                   "ChSearchRes"
                   [ ctor
                         "Right"
                         [ ifExpr
                               (eq tQuery (varRef "q" __LINE__) (ctor "Query1" [] __LINE__) __LINE__)
                               // Query hit.
                               (tuple2
                                   // Page to return.
                                   (ifExpr
                                       // i < (List.length p1) + 1
                                       (less
                                           tNat
                                           (varRef "i" __LINE__)
                                           (plus
                                               tNat
                                               (size tPages (varRef "PAGES1" __LINE__) __LINE__)
                                               (litNat 1u __LINE__)
                                               __LINE__)
                                           __LINE__)
                                       // Return requested page.
                                       // PAGES ! (i-1)
                                       (listNth
                                           (varRef "PAGES1" __LINE__)
                                           (minus tNat (varRef "i" __LINE__) (litNat 1u __LINE__) __LINE__)
                                           __LINE__)
                                       // Index out of range.
                                       (litEmpty tPage __LINE__)
                                       __LINE__)
                                   (less
                                       tNat
                                       (minus tNat (varRef "i" __LINE__) (litNat 1u __LINE__) __LINE__)
                                       (size tPages (varRef "PAGES1" __LINE__) __LINE__)
                                       __LINE__)
                                   __LINE__)
                               // Otherwise.
                               (tuple2 (litEmpty (tList tRepo) __LINE__) (litFalse __LINE__) __LINE__)
                               __LINE__ ]
                         __LINE__ ]
                   __LINE__)
               (unwind "GHSearch" [] __LINE__)
               __LINE__)

          (("GHStar", [ ("starRel", tStarRel) ]),
           extCh
               (prefixRecv
                   (univ tChChkStarReq __LINE__)
                   "t"
                   (``match``
                       (varRef "t" __LINE__)
                       [ (("ChChkStarReq", [ "repo"; "pat" ]),
                          unwind
                              "GHChkStarRecv1"
                              [ varRef "repo" __LINE__; varRef "pat" __LINE__; varRef "starRel" __LINE__ ]
                              __LINE__) ]
                       __LINE__)
                   __LINE__)
               (extCh
                   (prefixRecv
                       (univ tChStarReq __LINE__)
                       "req"
                       (``match``
                           (varRef "req" __LINE__)
                           [ (("ChStarReq", [ "repo"; "pat" ]),
                              unwind
                                  "GHStarRecv1"
                                  [ varRef "repo" __LINE__; varRef "pat" __LINE__; varRef "starRel" __LINE__ ]
                                  __LINE__) ]
                           __LINE__)
                       __LINE__)
                   (prefixRecv
                       (univ tChUnstarReq __LINE__)
                       "req"
                       (``match``
                           (varRef "req" __LINE__)
                           [ (("ChUnstarReq", [ "repo"; "pat" ]),
                              unwind
                                  "GHUnstarRecv1"
                                  [ varRef "repo" __LINE__; varRef "pat" __LINE__; varRef "starRel" __LINE__ ]
                                  __LINE__) ]
                           __LINE__)
                       __LINE__)
                   __LINE__)
               __LINE__)

          (("GHChkStarRecv1", [ ("repo", tRepo); ("pat", tPat); ("starRel", tStarRel) ]),
           ``match``
               (mapFindOpt (varRef "pat" __LINE__) (varRef "PAT_REL" __LINE__) __LINE__)
               [ (("Some", [ "user" ]),
                  unwind
                      "GHChkStarRecv2"
                      [ (varRef "repo" __LINE__)
                        (varRef "user" __LINE__)
                        (varRef "starRel" __LINE__) ]
                      __LINE__)
                 (("None", []), unwind "GHChkStarWillFail" [ varRef "starRel" __LINE__ ] __LINE__) ]
               __LINE__)

          (("GHChkStarRecv2", [ ("repo", tRepo); ("user", tUser); ("starRel", tStarRel) ]),
           intCh
               (unwind "GHChkStarWillFail" [ varRef "starRel" __LINE__ ] __LINE__)
               (unwind
                   "GHChkStarWillResp"
                   [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "starRel" __LINE__ ]
                   __LINE__)
               __LINE__)

          (("GHChkStarWillFail", [ ("starRel", tStarRel) ]),
           prefix
               (ctor "ChChkStarRes" [ ctor "Left" [ ctor "GHChkStarError" [] __LINE__ ] __LINE__ ] __LINE__)
               (unwind "GHStar" [ varRef "starRel" __LINE__ ] __LINE__)
               __LINE__)

          (("GHChkStarWillResp", [ ("repo", tRepo); ("user", tUser); ("starRel", tStarRel) ]),
           prefix
               (ctor
                   "ChChkStarRes"
                   [ ctor
                         "Right"
                         [ setMem
                               (tuple2 (varRef "repo" __LINE__) (varRef "user" __LINE__) __LINE__)
                               (varRef "starRel" __LINE__)
                               __LINE__ ]
                         __LINE__ ]
                   __LINE__)
               (unwind "GHStar" [ varRef "starRel" __LINE__ ] __LINE__)
               __LINE__)

          (("GHStarRecv1", [ ("repo", tRepo); ("pat", tPat); ("starRel", tStarRel) ]),
           ``match``
               (mapFindOpt (varRef "pat" __LINE__) (varRef "PAT_REL" __LINE__) __LINE__)
               [ (("Some", [ "user" ]),
                  unwind
                      "GHStarRecv2"
                      [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "starRel" __LINE__ ]
                      __LINE__)
                 (("None", []), unwind "GHStarWillFail" [ varRef "starRel" __LINE__ ] __LINE__) ]
               __LINE__)

          (("GHStarRecv2", [ ("repo", tRepo); ("user", tUser); ("starRel", tStarRel) ]),
           intCh
               (unwind "GHStarWillFail" [ varRef "starRel" __LINE__ ] __LINE__)
               (unwind
                   "GHStarWillResp"
                   [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "starRel" __LINE__ ]
                   __LINE__)
               __LINE__)

          (("GHStarWillFail", [ ("starRel", tStarRel) ]),
           prefix
               (ctor "ChStarRes" [ ctor "Left" [ ctor "GHStarError" [] __LINE__ ] __LINE__ ] __LINE__)
               (unwind "GHStar" [ varRef "starRel" __LINE__ ] __LINE__)
               __LINE__)

          (("GHStarWillResp", [ ("repo", tRepo); ("user", tUser); ("starRel", tStarRel) ]),
           prefix
               (ctor
                   "ChStarRes"
                   [ ctor
                         "Right"
                         [ boolNot
                               (setMem
                                   (tuple2 (varRef "repo" __LINE__) (varRef "user" __LINE__) __LINE__)
                                   (varRef "starRel" __LINE__)
                                   __LINE__)
                               __LINE__ ]
                         __LINE__ ]
                   __LINE__)
               (unwind
                   "GHStar"
                   [ setInsert
                         (tuple2 (varRef "repo" __LINE__) (varRef "user" __LINE__) __LINE__)
                         (varRef "starRel" __LINE__)
                         __LINE__ ]
                   __LINE__)
               __LINE__)

          (("GHUnstarRecv1", [ ("repo", tRepo); ("pat", tPat); ("starRel", tStarRel) ]),
           ``match``
               (mapFindOpt (varRef "pat" __LINE__) (varRef "PAT_REL" __LINE__) __LINE__)
               [ (("Some", [ "user" ]),
                  unwind
                      "GHUnstarRecv2"
                      [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "starRel" __LINE__ ]
                      __LINE__)
                 (("None", []), unwind "GHUnstarWillFail" [ varRef "starRel" __LINE__ ] __LINE__) ]
               __LINE__)

          (("GHUnstarRecv2", [ ("repo", tRepo); ("user", tUser); ("starRel", tStarRel) ]),
           intCh
               (unwind "GHUnstarWillFail" [ varRef "starRel" __LINE__ ] __LINE__)
               (unwind
                   "GHUnstarWillResp"
                   [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "starRel" __LINE__ ]
                   __LINE__)
               __LINE__)

          (("GHUnstarWillFail", [ ("starRel", tStarRel) ]),
           prefix
               (ctor "ChUnstarRes" [ ctor "Left" [ ctor "GHUnstarError" [] __LINE__ ] __LINE__ ] __LINE__)
               (unwind "GHStar" [ varRef "starRel" __LINE__ ] __LINE__)
               __LINE__)

          (("GHUnstarWillResp", [ ("repo", tRepo); ("user", tUser); ("starRel", tStarRel) ]),
           prefix
               (ctor
                   "ChUnstarRes"
                   [ ctor
                         "Right"
                         [ setMem
                               (tuple2 (varRef "repo" __LINE__) (varRef "user" __LINE__) __LINE__)
                               (varRef "starRel" __LINE__)
                               __LINE__ ]
                         __LINE__ ]
                   __LINE__)
               (unwind
                   "GHStar"
                   [ setRemove
                         (tuple2 (varRef "repo" __LINE__) (varRef "user" __LINE__) __LINE__)
                         (varRef "starRel" __LINE__)
                         __LINE__ ]
                   __LINE__)
               __LINE__)

          (("AppLaunch", [ ("pOpt", tOption tPat) ]),
           ``match``
               (varRef "pOpt" __LINE__)
               [ (("Some", [ "p" ]),
                  unwind "AppDispSearch" [ varRef "p" __LINE__; ctor "QueryEmpty" [] __LINE__ ] __LINE__)
                 (("None", []), unwind "AppDispLogin" [ ctor "PatEmpty" [] __LINE__ ] __LINE__) ]
               __LINE__)

          (("AppDispLogin", [ ("p1", tPat) ]),
           extCh
               (prefixRecv
                   (univ tChPatField __LINE__)
                   "ch"
                   (``match``
                       (varRef "ch" __LINE__)
                       [ (("ChPATField", [ "p2" ]), unwind "AppDispLogin" [ varRef "p2" __LINE__ ] __LINE__) ]
                       __LINE__)
                   __LINE__)
               (guard
                   (boolNot (eq tPat (varRef "p1" __LINE__) (ctor "PatEmpty" [] __LINE__) __LINE__) __LINE__)
                   (prefix
                       (ctor "EvLoginBtn" [] __LINE__)
                       (unwind "AppDidPressLoginBtn" [ varRef "p1" __LINE__ ] __LINE__)
                       __LINE__)
                   __LINE__)
               __LINE__)

          (("AppDidPressLoginBtn", [ ("pat", tPat) ]),
           prefix
               (ctor "ChAuthReq" [ varRef "pat" __LINE__ ] __LINE__)
               (unwind "AppReqAuth" [ varRef "pat" __LINE__ ] __LINE__)
               __LINE__)

          (("AppReqAuth", [ ("pat", tPat) ]),
           (prefixRecv
               (univ tChAuthRes __LINE__)
               "res"
               (``match``
                   (varRef "res" __LINE__)
                   [ (("ChAuthResError", [ "_" ]), (unwind "AppDialogAuthError" [ varRef "pat" __LINE__ ] __LINE__))
                     (("ChAuthResOk", [ "userOpt" ]),
                      (``match``
                          (varRef "userOpt" __LINE__)
                          [ (("Some", [ "_" ]), unwind "AppRecvAuth" [ varRef "pat" __LINE__ ] __LINE__)
                            (("None", []), unwind "AppDialogAuthFailed" [] __LINE__) ]
                          __LINE__)) ]
                   __LINE__)
               __LINE__))

          (("AppDialogAuthError", [ ("p", tPat) ]),
           prefix
               (ctor "ChDispLogin" [ ctor "AppAuthError" [] __LINE__ ] __LINE__)
               (unwind "AppDispLogin" [ varRef "p" __LINE__ ] __LINE__)
               __LINE__)

          (("AppDialogAuthFailed", []),
           prefix
               (ctor "ChDispLogin" [ ctor "AppAuthFailed" [] __LINE__ ] __LINE__)
               (unwind "AppDispLogin" [ ctor "PatEmpty" [] __LINE__ ] __LINE__)
               __LINE__)

          (("AppRecvAuth", [ ("p", tPat) ]),
           prefix
               (ctor "ChDispLogin" [ ctor "AppAuthSuccess" [] __LINE__ ] __LINE__)
               (unwind "AppDispSearch" [ varRef "p" __LINE__; ctor "QueryEmpty" [] __LINE__ ] __LINE__)
               __LINE__)

          (("AppDispSearch", [ ("q", tQuery); ("p", tPat) ]),
           extCh
               (prefix
                   (ctor "EvLogoutBtn" [] __LINE__)
                   (unwind "AppDispLogin" [ ctor "PatEmpty" [] __LINE__ ] __LINE__)
                   __LINE__)
               (extCh
                   (prefixRecv
                       (univ tChSearchField __LINE__)
                       "ch"
                       (``match``
                           (varRef "ch" __LINE__)
                           [ (("ChSearchField", [ "q" ]),
                              unwind "AppDispSearch" [ varRef "q" __LINE__; varRef "p" __LINE__ ] __LINE__) ]
                           __LINE__)
                       __LINE__)
                   (guard
                       (boolNot (eq tQuery (varRef "q" __LINE__) (ctor "QueryEmpty" [] __LINE__) __LINE__) __LINE__)
                       (prefix
                           (ctor "EvSearchBtn" [] __LINE__)
                           (unwind "AppDidPressSearchBtn" [ varRef "q" __LINE__; varRef "p" __LINE__ ] __LINE__)
                           __LINE__)
                       __LINE__)
                   __LINE__)
               __LINE__)
          (("AppDidPressSearchBtn", [ ("q", tQuery); ("p", tPat) ]),
           prefix
               (ctor "ChSearchReq" [ varRef "q" __LINE__; litNat 0u __LINE__ ] __LINE__)
               (unwind "AppReqSearch" [ varRef "q" __LINE__; varRef "p" __LINE__ ] __LINE__)
               __LINE__)
          (("AppReqSearch", [ ("q", tQuery); ("p", tPat) ]),
           prefixRecv
               (univ tChSearchRes __LINE__)
               "ch"
               (``match``
                   (varRef "ch" __LINE__)
                   [ (("ChSearchResError", [ "_" ]),
                      unwind "AppDialogSearchError" [ varRef "q" __LINE__; varRef "p" __LINE__ ] __LINE__)
                     (("ChSearchResOk", [ "repos"; "hasMore" ]),
                      unwind
                          "ARecvSearch"
                          [ varRef "repos" __LINE__; varRef "hasMore" __LINE__; varRef "p" __LINE__ ]
                          __LINE__) ]
                   __LINE__)
               __LINE__)
          (("AppDialogSearchError", [ ("q", tQuery); ("p", tPat) ]),
           prefix
               (ctor "ChDispSearchError" [] __LINE__)
               (unwind "AppDispSearch" [ varRef "q" __LINE__; varRef "p" __LINE__ ] __LINE__)
               __LINE__)
          (("AppRecvSearch", [ ("repos", tList tRepo); ("hasMore", tBool); ("p", tPat) ]),
           prefix
               (ctor
                   "ChDispSearchOk"
                   [ varRef "repos" __LINE__
                     varRef "hasMore" __LINE__
                     litEmpty (tMap tRepo (tOption tBool)) __LINE__ ]
                   __LINE__)
               (unwind
                   "AppDispSearchResult"
                   [ varRef "repos" __LINE__
                     varRef "hasMore" __LINE__
                     litEmpty (tMap tRepo (tOption tBool)) __LINE__
                     litNat 0u __LINE__
                     varRef "p" __LINE__ ]
                   __LINE__)
               __LINE__)
          (("AppDispSearchResult",
            [ ("repos", tRepo)
              ("hasMore", tBool)
              ("starMap", tMap tRepo (tOption tBool))
              ("page", tNat)
              ("p", tPat) ]),
           extCh
               (prefix
                   (ctor "EvMoreBtn" [] __LINE__)
                   (unwind
                       "AppDispPressMoreBtn"
                       [ varRef "repos" __LINE__
                         varRef "hasMore" __LINE__
                         varRef "starMap" __LINE__
                         varRef "page" __LINE__
                         varRef "p" __LINE__ ]
                       __LINE__)
                   __LINE__)
               (prefixRecv
                   (filter
                       (tSet tChChkStarBtn)
                       "ch"
                       (matchExpr
                           (varRef "ch" __LINE__)
                           [ (("ChChkStarBtn", [ "repo" ]),
                              listContains (varRef "repo" __LINE__) (varRef "repos" __LINE__) __LINE__) ]
                           __LINE__)
                       (univ tChChkStarBtn __LINE__)
                       __LINE__)
                   ""
                   (unwind
                       "AppDisPressChkStar"
                       [ varRef "repos" __LINE__
                         varRef "hasMore" __LINE__
                         varRef "starMap" __LINE__
                         varRef "page" __LINE__
                         varRef "p" __LINE__ ]
                       __LINE__)
                   __LINE__)
               __LINE__)
          (("AppDispPressMoreBtn",
            [ ("repos", tRepo)
              ("hasMore", tBool)
              ("starMap", tMap tRepo (tOption tBool))
              ("page", tNat)
              ("p", tPat) ]),
           stop __LINE__) // TODO: impl
          (("AppDispPressChkStar",
            [ ("repos", tRepo)
              ("hasMore", tBool)
              ("starMap", tMap tRepo (tOption tBool))
              ("page", tNat)
              ("p", tPat) ]),
           stop __LINE__) ] // TODO: impl
    |> ResultEx.get ProcMapError.format


let genv: Env =
    Env.from
        [ ("PAT_REL", vMap [ (vUnion "Pat1" [], vUnion "User1" []) ])
          ("PAGES1", vList [ vList [ vUnion "Repo1" []; vUnion "Repo2" [] ]; vList [ vUnion "Repo3" [] ] ]) ]
