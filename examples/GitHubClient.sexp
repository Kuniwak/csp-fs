(type event EvLoginBtn EvLogoutBtn EvSearchBtn EvMoreBtn)

(type pat Pat1 Pat2 PatEmpty)
(type query Query1 Query2 QueryEmpty)
(type user User1)
(type repo Repo1 Repo2 Repo3)
(type page Page1 Page2 Page3)

(type dispLogin AppAuthSuccess AppAuthError AppAuthFailed)
(type dispSearchError AppSearchError AppSearchMoreError AppChkStarError AppStarError AppUnstarError)

(type chAuthReq (ChAuthReq pat))
(type chSearchReq (ChSearchReq query nat))
(type chChkStarReq (ChChkStarReq repo pat))
(type chStarReq (ChStarReq repo pat))
(type chUnstarReq (ChUnstarReq repo pat))

(type chAuthRes ChAuthResError (ChAuthResOk (option user)))
(type chSearchRes ChSearchResError (ChSearchResOk (list repo) bool))
(type chChkStarRes ChChkStarResError (ChChkStarResOk bool))
(type chStarRes ChStarResError (ChStarResOk bool))
(type chUnstarRes ChUnstarResError (ChUnstarResOk bool))

(type chPatField (ChPatField pat))
(type chSearchField (ChSearchField pat))
(type chChkStarBtn (ChChkStarBtn repo))
(type chStarBtn (ChStarBtn repo))
(type chUnstarBtn (ChUnstarBtn repo))

(type chDispLogin (ChDispLogin dispLogin))
(type chDispSearch ChDispSearchError (ChDispSearchOk (list repo) bool (map repo (option bool))))


(const patRel (add Pat1 User1 (empty (map pat user))))
(const page1 (cons Repo1 (cons Repo2 (empty (list repo)))))
(const page2 (cons Repo3 (empty (list repo))))
(const noPage (empty (list repo)))


(proc GHAuth ()
  (prefixRecv (univ chAuthReq) ch (match ch (ChAuthReq p (unwind GHAuthRecv p)))))

(proc GHAuthRecv ((p pat))
  (in (unwind GHAuthWillFail) (unwind GHAuthWillResp p)))

(proc GHAuthWillFail ()
  (prefix ChAuthResError (unwind GHAuth)))

(proc GHAuthWillResp ((p pat))
  (prefix (ChAuthResOk (findOpt p patRel)) (unwind GHAuth)))

(proc GHSearch ()
  (prefixRecv (univ chSearchReq) ch (match ch (ChSearchReq q i (unwind GHSearchRecv q i)))))

(proc GHSearchRecv ((q query) (i page))
  (in (unwind GHSearchWillFail) (unwind GHSearchWillResp q i)))

(proc GHSearchWillFail ()
  (prefix ChSearchResError (unwind GHSearch)))

(proc GHSearchWillResp ((q query) (i page))
  (prefix
    (match q
      (Query1 
       (match i
        (Page1 (ChSearchResOk page1 true))
        (Page2 (ChSearchResOk page2 false))
        (Page3 (ChSearchResOk noPage false))))
      (Query2 (ChSearchResOk noPage false))
      (QueryEmpty (ChSearchResOk noPage false)))
    (unwind GHSearch)))
