module CSP.Model.GHApp

open CSP.Core
open CSP.Core.Env
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.ValShorthand
open CSP.Core.ProcMap

let tEvent =
    tUnion "event" [ ("EvLoginBtn", []); ("EvLogoutBtn", []); ("EvSearchBtn", []) ]

let tPat = tUnion "pat" [ ("Pat1", []); ("Pat2", []); ("PatEmpty", []) ]

let tQuery =
    tUnion "query" [ ("Query1", []); ("Query2", []); ("QueryEmpty", []) ]

let tUser = tUnion "user" [ ("User1", []) ]
let tRepo = tUnion "repo" [ ("Repo1", []); ("Repo2", []); ("Repo3", []) ]

let tGHAuthError = tUnion "ghAuthError" [ ("GHAuthError", []) ]
let tGHSearchError = tUnion "ghSearchError" [ ("GHSearchError", []) ]

let tGHSearchMoreError =
    tUnion "ghSearchMoreError" [ ("GHSearchMoreError", []) ]

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
    tUnion "tChAuthRes" [ ("ChAuthRes", [ tEither tGHAuthError (tOption tUser) ]) ]

let tChSearchReq =
    tUnion "tChSearchReq" [ ("ChSearchReq", [ tTuple2 tQuery tNat ]) ]

let tChSearchRes =
    tUnion
        "tChSearchRes"
        [ ("ChSearchRes", [ tEither tGHSearchError (tTuple2 (tList tRepo) tBool) ]) ]
       

let tChChkStarReq =
    tUnion "tChChkStarReq" [ ("ChChkStarReq", [ tTuple2 tRepo tPat ]) ]

let tChChkStarRes =
    tUnion "tChChkStarRes" [ ("ChChkStarRes", [ tEither tGHChkStarError tBool ]) ]

let tChStarReq =
    tUnion "tChStarReq" [ ("ChStarReq", [ tTuple2 tRepo tPat ]) ]

let tChStarRes =
    tUnion "tChStarRes" [ ("ChStarRes", [ tEither tGHStarError tBool ]) ]

let tChUnstarReq =
    tUnion "tChUnstarReq" [ ("ChUnstarReq", [ tTuple2 tRepo tPat ]) ]

let tChUnstarRes =
    tUnion "tChUnstarRes" [ ("ChUnstarRes", [ tEither tGHUnstarError tBool ]) ]

let tChPatField = tUnion "tChPATField" [ ("ChPATField", [ tPat ]) ]

let tChSearchField =
    tUnion "tChSearchField" [ ("ChSearchField", [ tQuery ]) ]

let tChChkStarBtn = tUnion "tChChkStarBtn" [ ("ChChkStarBtn", [ tRepo ]) ]
let tChStarBtn = tUnion "tChStarBtn" [ ("ChStarBtn", [ tRepo ]) ]
let tChUnstarBtn = tUnion "tChUnstarBtn" [ ("ChUnstarBtn", [ tRepo ]) ]

let tChDispLogin = tUnion "tChDispLogin" [ ("ChDispLogin", [ tDispLogin ]) ]

let tChDispSearch =
    tUnion
        "tChDispSearch"
        [ ("ChDispSearch", [ (tEither tDispSearchError (tTuple3 (tList tRepo) (tMap tRepo (tOption tBool)) tBool)) ]) ]

let tPage = tList tRepo
let tPages = tList tPage


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
    ResultEx.get
        ProcMapError.format
        (from
            [ (("GHAuth", []),
               prefixRecv (univ tChAuthReq __LINE__) "p" (unwind "GHAuthRecv" [ varRef "p" __LINE__ ] __LINE__) __LINE__)
              (("GHAuthRecv", [ "p" ]),
               intCh
                   (unwind "GHAuthWillFail" [ varRef "p" __LINE__ ] __LINE__)
                   (unwind "GHAuthWillResp" [ varRef "p" __LINE__ ] __LINE__)
                   __LINE__)
              (("GHAuthWillFail", [ "p" ]),
               prefix
                   (ctor "ChAuthRes" [ (ctor "Left" [ ctor "GHAuthError" [] __LINE__ ] __LINE__) ] __LINE__)
                   (unwind "GHAuth" [] __LINE__)
                   __LINE__)
              (("GHAuthWillResp", [ "p" ]),
               (prefix
                   (ctor
                       "ChAuthRes"
                       [ (ctor
                             "Right"
                             [ mapFindOpt (varRef "p" __LINE__) (varRef "PAT_REL" __LINE__) __LINE__ ]
                             __LINE__) ]
                       __LINE__)
                   (unwind "GHAuth" [] __LINE__))
                   __LINE__)

              (("GHSearch", []),
               prefixRecv
                   (univ tChSearchReq __LINE__)
                   "r"
                   (``match``
                       (varRef "r" __LINE__)
                       [ ((Some "ChSearchReq", [ "q"; "i" ]),
                          unwind "GHSearchRecv" [ (varRef "q" __LINE__); (varRef "i" __LINE__) ] __LINE__) ]
                       __LINE__)
                   __LINE__)
              (("GHSearchRecv", [ "q"; "i" ]),
               intCh
                   (unwind "GHSearchWillFail" [] __LINE__)
                   (unwind "GHSearchWillResp" [ varRef "q" __LINE__; varRef "i" __LINE__ ] __LINE__)
                   __LINE__)
              (("GHSearchWillFail", []),
               prefix
                   (ctor "ChSearchRes" [ ctor "Left" [ ctor "GHSearchError" [] __LINE__ ] __LINE__ ] __LINE__)
                   (unwind "GHSearch" [] __LINE__)
                   __LINE__)
              (("GHSearchWillResp", [ "q"; "i" ]),
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
                                           (varRef "i" __LINE__)
                                           (minus
                                               tNat
                                               (size tPages (varRef "PAGES1" __LINE__) __LINE__)
                                               (litNat 1u __LINE__)
                                               __LINE__)
                                           __LINE__)
                                       __LINE__)
                                   // Otherwise.
                                   (tuple2 (litEmpty (tList tRepo) __LINE__) (litFalse __LINE__) __LINE__)
                                   __LINE__ ]
                             __LINE__ ]
                       __LINE__)
                   (unwind "GHSearch" [] __LINE__)
                   __LINE__)

              (("GHStar", [ "starRel" ]),
               extCh
                   (prefixRecv
                       (univ tChChkStarReq __LINE__)
                       "t"
                       (``match``
                           (varRef "t" __LINE__)
                           [ ((Some "ChChkStarReq", [ "repo"; "pat" ]),
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
                               [ ((Some "ChStarReq", [ "repo"; "pat" ]),
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
                               [ ((Some "ChUnstarReq", [ "repo"; "pat" ]),
                                  unwind
                                      "GHUnstarRecv1"
                                      [ varRef "repo" __LINE__; varRef "pat" __LINE__; varRef "starRel" __LINE__ ]
                                      __LINE__) ]
                               __LINE__)
                           __LINE__)
                       __LINE__)
                   __LINE__)
              (("GHChkStarRecv1", [ "repo"; "pat"; "starRel" ]),
               ``match``
                   (mapFindOpt (varRef "pat" __LINE__) (varRef "PAT_REL" __LINE__) __LINE__)
                   [ ((Some "Some", [ "user" ]),
                      unwind
                          "GHChkStarRecv2"
                          [ (varRef "repo" __LINE__)
                            (varRef "user" __LINE__)
                            (varRef "starRel" __LINE__) ]
                          __LINE__)
                     ((Some "None", []), unwind "GHChkStarWillFail" [] __LINE__) ]
                   __LINE__)
              (("GHChkStarRecv2", [ "repo"; "user"; "starRel" ]),
               intCh
                   (unwind "GHChkStarWillFail" [ varRef "starRel" __LINE__ ] __LINE__)
                   (unwind
                       "GHChkStarWillResp"
                       [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "starRel" __LINE__ ]
                       __LINE__)
                   __LINE__)
              (("GHChkStarWillFail", [ "starRel" ]),
               prefix
                   (ctor "ChChkStarRes" [ ctor "Left" [ ctor "GHChkStarError" [] __LINE__ ] __LINE__ ] __LINE__)
                   (unwind "GHStar" [ varRef "starRel" __LINE__ ] __LINE__)
                   __LINE__)
              (("GHChkStarWillResp", [ "repo"; "user"; "starRel" ]),
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
                   (unwind "GHStar" [] __LINE__)
                   __LINE__)

              (("GHStarRecv1", [ "repo"; "pat"; "starRel" ]),
               ``match``
                   (mapFindOpt (varRef "pat" __LINE__) (varRef "PAT_REL" __LINE__) __LINE__)
                   [ ((Some "Some", [ "user" ]),
                      unwind
                          "GHStarRecv2"
                          [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "u" __LINE__ ]
                          __LINE__)
                     ((Some "None", []), unwind "GHStarWillFail" [] __LINE__) ]
                   __LINE__)

              (("GHStarRecv2", [ "repo"; "user"; "starRel" ]),
               intCh
                   (unwind "GHStarWillFail" [ varRef "starRel" __LINE__ ] __LINE__)
                   (unwind "GHStarWillResp" [ varRef "t" __LINE__ ] __LINE__)
                   __LINE__)
              (("GHStarWillFail", [ "starRel" ]),
               prefix
                   (ctor "ChStarRes" [ ctor "Left" [ ctor "GHStarError" [] __LINE__ ] __LINE__ ] __LINE__)
                   (unwind "GHStar" [ varRef "starRel" __LINE__ ] __LINE__)
                   __LINE__)
              (("GHStarWillResp", [ "repo"; "user" ]),
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
                   (unwind "GHStar" [ setInsert (varRef "t" __LINE__) (varRef "starRel" __LINE__) __LINE__ ] __LINE__)
                   __LINE__)

              (("GHUnstarRecv1", [ "repo"; "pat"; "starRel" ]),
               ``match``
                   (mapFindOpt (varRef "pat" __LINE__) (varRef "PAT_REL" __LINE__) __LINE__)
                   [ ((Some "Some", [ "user" ]),
                      unwind
                          "GHUnstarRecv2"
                          [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "u" __LINE__ ]
                          __LINE__)
                     ((Some "None", []), unwind "GHUnstarWillFail" [] __LINE__) ]
                   __LINE__)
              (("GHUnstarRecv2", [ "repo"; "user"; "starRel" ]),
               intCh
                   (unwind "GHUnstarWillFail" [] __LINE__)
                   (unwind
                       "GHUnstarWillResp"
                       [ varRef "repo" __LINE__; varRef "user" __LINE__; varRef "starRel" __LINE__ ]
                       __LINE__)
                   __LINE__)

              (("GHUnstarWillFail", []),
               prefix
                   (ctor "ChStarRes" [ ctor "Left" [ ctor "GHUnstarError" [] __LINE__ ] __LINE__ ] __LINE__)
                   (unwind "GHStar" [ varRef "starRel" __LINE__ ] __LINE__)
                   __LINE__)

              (("GHUnstarWillResp", [ "repo"; "user"; "starRel" ]),
               prefix
                   (ctor
                       "ChStarRes"
                       [ ctor
                             "Right"
                             [ setMem
                                   (tuple2 (varRef "repo" __LINE__) (varRef "user" __LINE__) __LINE__)
                                   (varRef "starRel" __LINE__)
                                   __LINE__ ]
                             __LINE__ ]
                       __LINE__)
                   (unwind "GHStar" [ setInsert (varRef "t" __LINE__) (varRef "starRel" __LINE__) __LINE__ ] __LINE__)
                   __LINE__)

              (("AppLaunch", [ "pOpt" ]),
               ``match``
                   (varRef "pOpt" __LINE__)
                   [ ((Some "Some", [ "p" ]), unwind "AppDispSearch" [ varRef "p" __LINE__ ] __LINE__)
                     ((Some "None", []), unwind "AppDispLogin" [ ctor "PatEmpty" [] __LINE__ ] __LINE__) ]
                   __LINE__)
              (("AppDispLogin", [ "p1" ]),
               extCh
                   (prefixRecv
                       (univ tChPatField __LINE__)
                       "p2"
                       (unwind "AppDispLogin" [ varRef "p2" __LINE__ ] __LINE__)
                       __LINE__)
                   (guard
                       (boolNot (eq tPat (varRef "p1" __LINE__) (ctor "PatEmpty" [] __LINE__) __LINE__) __LINE__)
                       (prefix
                           (ctor "EvLoginBtn" [] __LINE__)
                           (unwind "AppDidPressLoginBtn" [ varRef "p1" __LINE__ ] __LINE__)
                           __LINE__)
                       __LINE__)
                   __LINE__)
              (("AppDidPressLoginBtn", [ "p" ]),
               prefix
                   (ctor "ChAuthReq" [ varRef "p" __LINE__ ] __LINE__)
                   (unwind "AppReqAuth" [ varRef "p" __LINE__ ] __LINE__)
                   __LINE__)
              (("AppReqAuth", [ "p" ]),
               ``match``
                   (varRef "p" __LINE__)
                   [ ((Some "Left", [ "_" ]), unwind "AppDialogAuthError" [ varRef "p" __LINE__ ] __LINE__)
                     ((Some "Right", [ "b" ]),
                      ``if``
                          (varRef "b" __LINE__)
                          (unwind "AppRecvAuth" [ varRef "p" __LINE__ ] __LINE__)
                          (unwind "AppDialogAuthFailed" [] __LINE__)
                          __LINE__) ]
                   __LINE__)
              (("AppDialogAuthError", [ "p" ]),
               prefix
                   (ctor "ChDispLogin" [ ctor "AppAuthError" [] __LINE__ ] __LINE__)
                   (unwind "AppDispLogin" [ varRef "p" __LINE__ ] __LINE__)
                   __LINE__)
              (("AppDialogAuthFailed", []),
               prefix
                   (ctor "ChDispLogin" [ ctor "AppAuthFailed" [] __LINE__ ] __LINE__)
                   (unwind "appDispLogin" [ ctor "PatEmpty" [] __LINE__ ] __LINE__)
                   __LINE__)
              (("AppRecvAuth", [ "p" ]),
               prefix
                   (ctor "ChDispLogin" [ ctor "AppAuthSuccess" [] __LINE__ ] __LINE__)
                   (unwind "AppDispSearch" [ ctor "QueryEmpty" [] __LINE__ ] __LINE__)
                   __LINE__)
              (("AppDispSearch", [ "q" ]),
               extCh
                   (prefix
                       (ctor "EvLogoutBtn" [] __LINE__)
                       (unwind "AppDispLogin" [ ctor "PatEmpty" [] __LINE__ ] __LINE__)
                       __LINE__)
                   (extCh
                       (prefixRecv
                           (univ tChSearchField __LINE__)
                           "q"
                           (unwind "AppDispSearch" [ varRef "q" __LINE__ ] __LINE__)
                           __LINE__)
                       (guard
                           (boolNot (eq tQuery (varRef "q" __LINE__) (ctor "QueryEmpty" [] __LINE__) __LINE__) __LINE__)
                           (prefix
                               (ctor "EvSearchBtn" [] __LINE__)
                               (unwind "AppDidPressSearchBtn" [ varRef "q" __LINE__ ] __LINE__)
                               __LINE__)
                           __LINE__)
                       __LINE__)
                   __LINE__) ])

let genv: Env =
    Env.from
        [ ("PAT_REL", vMap [ (vUnion "Pat1" [], vUnion "User1" []) ])
          ("PAGES1", vList [ vList [ vUnion "Repo1" []; vUnion "Repo2" [] ]; vList [ vUnion "Repo3" [] ] ]) ]
