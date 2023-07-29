module CSP.Model.GHApp

open CSP.Core
open CSP.Core.Env
open CSP.Core.Type
open CSP.Core.ProcShorthand
open CSP.Core.ExprShorthand
open CSP.Core.TypeShorthand
open CSP.Core.ValShorthand
open CSP.Core.ProcMap

let tEvent =
    tUnion "event" [ ("EvLoginBtn", []); ("EvLogoutBtn", []); ("EvSearchBtn", []) ]

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

let tDispSearch =
    tEither tDispSearchError (tTuple [ tList tRepo; tMap tRepo (tOption tBool); TBool ])

let tChAuthReq = tUnion "tChAuthReq" [ ("ChAuthReq", [ tPat ]) ]

let tChAuthRes =
    tUnion "tChAuthRes" [ ("ChAuthRes", [ tEither tGHAuthError (tOption tUser) ]) ]

let tChSearchReq =
    tUnion "tChSearchReq" [ ("ChSearchReq", [ tTuple2 tQuery tNat ]) ]

let tChSearchRes =
    tUnion "tChSearchRes" [ ("ChSearchRes", [ tEither tGHSearchError (tTuple2 (TList tRepo) TBool) ]) ]

let tChChkStarReq =
    tUnion "tChChkStarReq" [ ("ChChkStarReq", [ tTuple2 tRepo tPat ]) ]

let tChChkStarRes =
    tUnion "tChChkStarRes" [ ("ChChkStarRes", [ tEither tGHChkStarError tBool ]) ]

let tChStarReq = tUnion "tChStarReq" [ ("ChStarReq", [ tTuple2 tRepo tPat ]) ]

let tChStarRes =
    tUnion "tChStarRes" [ ("ChStarRes", [ tEither tGHStarError TBool ]) ]

let tChUnstarReq = tUnion "tChUnstarReq" [ ("ChUnstarReq", [ tTuple2 tRepo tPat ]) ]

let tChUnstarRes =
    tUnion "tChUnstarRes" [ ("ChUnstarRes", [ tEither tGHUnstarError tBool ]) ]

let tChPatField = tUnion "tChPATField" [ ("ChPATField", [ tPat ]) ]
let tChSearchField = tUnion "tChSearchField" [ ("ChSearchField", [ tQuery ]) ]
let tChChkStarBtn = tUnion "tChChkStarBtn" [ ("ChChkStarBtn", [ tRepo ]) ]
let tChStarBtn = tUnion "tChStarBtn" [ ("ChStarBtn", [ tRepo ]) ]
let tChUnstarBtn = tUnion "tChUnstarBtn" [ ("ChUnstarBtn", [ tRepo ]) ]
let tChDispLogin = tUnion "tChDispLogin" [ ("ChDispLogin", [ tDispLogin ]) ]

let tChDispSearch = tUnion "tChDispSearch" [ ("ChDispSearch", [ tDispSearch ]) ]
let tPage = tList tRepo
let tPages = tList tPage


let ctorMap =
    CtorMap.from
        [ tEvent
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
          tChPatField
          tChSearchField
          tChChkStarBtn
          tChStarBtn
          tChUnstarBtn
          tChDispLogin
          tChDispSearch ]

let procMap =
    from
        [ (("GHAuth", []), prefixRecv (univ tChAuthReq) "p" (unwind "GHAuthRecv" [ varRef "p" ]))
          (("GHAuthRecv", [ "p" ]),
           intCh (unwind "GHAuthWillFail" [ varRef "p" ]) (unwind "GHAuthWillResp" [ varRef "p" ]))
          (("GHAuthWillFail", [ "p" ]),
           prefix (ctor "ChAuthRes" [ (ctor "Left" [ ctor "GHAuthError" [ litUnit ] ]) ]) (unwind "GHAuth" []))
          (("GHAuthWillResp", [ "p" ]),
           (prefix
               (ctor "ChAuthRes" [ (ctor "Right" [ mapFindOpt (varRef "p") (varRef "PAT_REL") ]) ])
               (unwind "GHAuth" [])))

          (("GHSearch", []), prefixRecv (univ tChSearchReq) "r" (unwind "GHSearchRecv" [ varRef "r" ]))
          (("GHSearchRecv", [ "r" ]), intCh (unwind "GHSearchWillFail" []) (unwind "GHSearchWillResp" [ varRef "r" ]))
          (("GHSearchWillFail", []),
           prefix (ctor "ChSearchRes" [ ctor "Left" [ ctor "GHSearchError" [] ] ]) (unwind "GHSearch" []))
          (("GHSearchWillResp", [ "r" ]),
           prefix
               (ctor
                   "ChSearchRes"
                   [ ctor
                         "Right"
                         [ (ifExpr
                               (eq tQuery (tupleFst (varRef "r")) (ctor "Query1" []))
                               // Q1 Hit.
                               (tuple2
                                   (ifExpr
                                       // fst r < List.length p1
                                       (less
                                           tNat
                                           (tupleSnd (varRef "r"))
                                           (plus tNat (size tPages (varRef "PAGES1")) (litNat 1u)))
                                       // Requested page.
                                       (listNth (varRef "PAGES1") (plus tNat (tupleSnd (varRef "r")) (litNat 1u)))
                                       // Out of range.
                                       (litEmpty tPage))
                                   (less
                                       tNat
                                       (tupleSnd (varRef "r"))
                                       (minus tNat (size tPages (varRef "PAGES1")) (litNat 1u))))
                               // Others.
                               (tuple2 (litEmpty (tList tRepo)) litFalse)) ] ])
               (unwind "GHSearch" []))

          (("GHStar", [ "starRel" ]),
           extCh
               (prefixRecv (univ tChChkStarReq) "t" (unwind "GHChkStarRecv1" [ varRef "t" ]))
               (extCh
                   (prefixRecv (univ tChStarReq) "t" (unwind "GHStarRecv1" [ varRef "t" ]))
                   (prefixRecv (univ tChUnstarReq) "t" (unwind "GHUnstarRecv1" [ varRef "t" ]))))
          (("GHChkStarRecv1", [ "t" ]),
           ``match``
               (mapFindOpt (tupleSnd (varRef "t")) (varRef "PAT_REL"))
               [ ((Some "Some", [ "u" ]), unwind "GHChkStarRecv2" [ tuple2 (tupleFst (varRef "t")) (varRef "u") ])
                 ((Some "None", []), unwind "GHChkStarWillFail" []) ])
          (("GHChkStarRecv2", [ "t" ]),
           intCh (unwind "GHChkStarWillFail" []) (unwind "GHChkStarWillResp" [ varRef "t" ]))
          (("GHChkStarWillFail", []),
           prefix
               (ctor "ChChkStarRes" [ ctor "Left" [ ctor "GHChkStarError" [] ] ])
               (unwind "GHStar" [ varRef "starRel" ]))
          (("GHChkStarWillResp", [ "t" ]),
           prefix (ctor "ChChkStarRes" [ ctor "Right" [ setMem (varRef "t") (varRef "starRel") ] ]) (unwind "GHStar" []))

          (("GHStarRecv1", [ "t" ]),
           ``match``
               (mapFindOpt (tupleSnd (varRef "t")) (varRef "PAT_REL"))
               [ ((Some "Some", [ "u" ]), unwind "GHStarRecv2" [ tuple2 (tupleFst (varRef "t")) (varRef "u") ])
                 ((Some "None", []), unwind "GHStarWillFail" []) ])

          (("GHStarRecv2", [ "t" ]), intCh (unwind "GHStarWillFail" []) (unwind "GHStarWillResp" [ varRef "t" ]))
          (("GHStarWillFail", []),
           prefix (ctor "ChStarRes" [ ctor "Left" [ ctor "GHStarError" [] ] ]) (unwind "GHStar" [ varRef "starRel" ]))
          (("GHStarWillResp", [ "t" ]),
           prefix
               (ctor "ChStarRes" [ ctor "Right" [ boolNot (setMem (varRef "t") (varRef "starRel")) ] ])
               (unwind "GHStar" [ setInsert (varRef "t") (varRef "starRel") ]))

          (("GHUnstarRecv2", [ "t" ]),
           ``match``
               (mapFindOpt (tupleSnd (varRef "t")) (varRef "PAT_REL"))
               [ ((Some "Some", [ "u" ]), unwind "GHUnstarRecv2" [ tuple2 (tupleFst (varRef "t")) (varRef "u") ])
                 ((Some "None", []), unwind "GHUnstarWillFail" []) ])
          (("GHUnstarRecv2", [ "t" ]), intCh (unwind "GHUnstarWillFail" []) (unwind "GHUnstarWillResp" [ varRef "t" ]))
          (("GHUnstarWillFail", []),
           prefix (ctor "ChStarRes" [ ctor "Left" [ ctor "GHUnstarError" [] ] ]) (unwind "GHStar" [ varRef "starRel" ]))
          (("GHUnstarWillResp", [ "t" ]),
           prefix
               (ctor "ChStarRes" [ ctor "Right" [ setMem (varRef "t") (varRef "starRel") ] ])
               (unwind "GHStar" [ setInsert (varRef "t") (varRef "starRel") ]))

          (("AppLaunch", [ "pOpt" ]),
           ``match``
               (varRef "pOpt")
               [ ((Some "Some", [ "p" ]), unwind "AppDispSearch" [ varRef "p" ])
                 ((Some "None", []), unwind "AppDispLogin" [ ctor "PatEmpty" [] ]) ])
          (("AppDispLogin", [ "p1" ]),
           extCh
               (prefixRecv (univ tChPatField) "p2" (unwind "AppDispLogin" [ varRef "p2" ]))
               (guard
                   (boolNot (eq tPat (varRef "p1") (ctor "PatEmpty" [])))
                   (prefix (ctor "EvLoginBtn" []) (unwind "AppDidPressLoginBtn" [ varRef "p1" ]))))
          (("AppDidPressLoginBtn", [ "p" ]),
           prefix (ctor "ChAuthReq" [ varRef "p" ]) (unwind "AppReqAuth" [ varRef "p" ]))
          (("AppReqAuth", [ "p" ]),
           ``match``
               (varRef "p")
               [ ((Some "Left", [ "_" ]), unwind "AppDialogAuthError" [ varRef "p" ])
                 ((Some "Right", [ "b" ]),
                  ``if`` (varRef "b") (unwind "AppRecvAuth" [ varRef "p" ]) (unwind "AppDialogAuthFailed" [])) ])
          (("AppDialogAuthError", [ "p" ]),
           prefix (ctor "ChDispLogin" [ ctor "AppAuthError" [] ]) (unwind "AppDispLogin" [ varRef "p" ]))
          (("AppDialogAuthFailed", []),
           prefix (ctor "ChDispLogin" [ ctor "AppAuthFailed" [] ]) (unwind "appDispLogin" [ ctor "PatEmpty" [] ]))
          (("AppRecvAuth", [ "p" ]),
           prefix
               (ctor "ChDispLogin" [ ctor "AppAuthSuccess" [] ])
               (unwind "AppDispSearch" [ ctor "QueryEmpty" [] ]))
          (("AppDispSearch", [ "q" ]),
           extCh
               (prefix (ctor "EvLogoutBtn" []) (unwind "AppDispLogin" [ ctor "PatEmpty" [] ]))
               (extCh
                   (prefixRecv (univ tChSearchField) "q" (unwind "AppDispSearch" [ varRef "q" ]))
                   (guard
                       (boolNot (eq tQuery (varRef "q") (ctor "QueryEmpty" [])))
                       (prefix (ctor "EvSearchBtn" []) (unwind "AppDidPressSearchBtn" [ varRef "q" ]))))) ]

let genv: Env =
    Env.from
        [ ("PAT_REL", vMap [ (vUnion "Pat1" [], vUnion "User1" []) ])
          ("PAGES1", vList [ vList [ vUnion "Repo1" []; vUnion "Repo2" [] ]; vList [ vUnion "Repo3" [] ] ]) ]
