(type event EvLoginBtn EvLogoutBtn EvSearchBtn EvMoreBtn)

(type pat Pat1 Pat2 PatEmpty)
(type query Query1 Query2 QueryEmpty)
(type user User1)
(type repo Repo1 Repo2 Repo3)
(type page Page1 Page2 Page3)

(type dispLogin AppAuthSuccess AppAuthError AppAuthFailed)
(type dispSearchError AppSearchError AppSearchMoreError AppChkStarError AppStarError AppUnstarError)

(type chAuthReq (ChAuthReq pat))
(type chSearchReq (ChSearchReq query page))
(type chChkStarReq (ChChkStarReq repo pat))
(type chStarReq (ChStarReq repo pat))
(type chUnstarReq (ChUnstarReq repo pat))

(type chAuthRes ChAuthResError (ChAuthResOk (option user)))
(type chSearchRes ChSearchResError (ChSearchResOk (list repo) bool))
(type chChkStarRes ChChkStarResError (ChChkStarResOk bool))
(type chStarRes ChStarResError (ChStarResOk bool))
(type chUnstarRes ChUnstarResError (ChUnstarResOk bool))

(type chPatField (ChPatField pat))
(type chSearchField (ChSearchField query))
(type chChkStarBtn (ChChkStarBtn repo))
(type chStarBtn (ChStarBtn repo))
(type chUnstarBtn (ChUnstarBtn repo))

(type chDispLogin (ChDispLogin dispLogin))
(type chDispSearch ChDispSearchError (ChDispSearchOk (list repo) bool query (map repo (option bool))))


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

(proc GHStar ((starRel (set (tuple repo user))))
  (ex
    (prefixRecv (univ chChkStarReq) ch
      (match ch
        (ChChkStarReq repo pat (unwind GHChkStarRecv repo pat starRel))))
    (prefixRecv (univ chStarReq) ch
      (match ch
        (ChStarReq repo pat (unwind GHStarRecv repo pat starRel))))
    (prefixRecv (univ chUnstarReq) ch
      (match ch
        (ChUnstarReq repo pat (unwind GHUnstarRecv repo pat starRel))))))

(proc GHChkStarRecv ((repo repo) (pat pat) (starRel (set (tuple repo user))))
  (match (findOpt pat patRel)
    (Some user
      (in
        (unwind GHChkStarWillFail starRel)
        (unwind GHChkStarWillResp repo user starRel)))
    (None
      (unwind GHChkStarWillFail starRel))))

(proc GHChkStarWillFail ((starRel (set (tuple repo user))))
  (prefix ChChkStarResError (unwind GHStar starRel)))

(proc GHChkStarWillResp ((repo repo) (user user) (starRel (set (tuple repo user))))
  (prefix
    (ChChkStarResOk (contains (set (tuple repo user)) (tuple repo user) starRel))
    (unwind GHStar starRel)))

(proc GHStarRecv ((repo repo) (pat pat) (starRel (set (tuple repo user))))
  (match (findOpt pat patRel)
    (Some user
      (in
        (unwind GHStarWillFail starRel)
        (unwind GHStarWillResp repo user starRel)))
    (None
      (unwind GHStarWillFail starRel))))

(proc GHStarWillFail ((starRel (set (tuple repo user))))
  (prefix ChStarResError (unwind GHStar starRel)))

(proc GHStarWillResp ((repo repo) (user user) (starRel (set (tuple repo user))))
  (prefix
    (ChStarResOk (not (contains (set (tuple repo user)) (tuple repo user) starRel)))
    (unwind GHStar (insert (tuple repo user) starRel))))

(proc GHUnstarRecv ((repo repo) (pat pat) (starRel (set (tuple repo user))))
  (match (findOpt pat patRel)
    (Some user
      (in
        (unwind GHUnstarWillFail starRel)
        (unwind GHUnstarWillResp repo user starRel)))
    (None
      (unwind GHUnstarWillFail starRel))))

(proc GHUnstarWillFail ((starRel (set (tuple repo user))))
  (prefix ChUnstarResError (unwind GHStar starRel)))

(proc GHUnstarWillResp ((repo repo) (user user) (starRel (set (tuple repo user))))
  (prefix
    (ChUnstarResOk (contains (set (tuple repo user)) (tuple repo user) starRel))
    (unwind GHStar (remove (tuple repo user) starRel))))


(proc AppLaunch ((pOpt (option pat)))
  (match pOpt
    (Some p (unwind AppDispSearch QueryEmpty p))
    (None (unwind AppDispLogin PatEmpty))))

(proc AppDispLogin ((p pat))
  (ex
    (prefixRecv (univ chPatField) ch
      (match ch
        (ChPatField p (unwind AppDispLogin p))))
    (guard (not (eq pat p PatEmpty))
      (prefix EvLoginBtn (unwind AppDidPressLoginBtn p)))))

(proc AppDidPressLoginBtn ((p pat))
  (prefix (ChAuthReq p) (unwind AppReqAuth p)))

(proc AppReqAuth ((p pat))
  (prefixRecv (univ chAuthRes) ch
    (match ch
      (ChAuthResError (unwind AppDialogAuthError p))
      (ChAuthResOk userOpt
        (match userOpt
          (Some _ (unwind AppRecvAuth p))
          (None (unwind AppDialogAuthFailed)))))))

(proc AppDialogAuthError ((p pat))
  (prefix (ChDispLogin AppAuthError) (unwind AppDispLogin p)))

(proc AppDialogAuthFailed ()
  (prefix (ChDispLogin AppAuthFailed) (unwind AppDispLogin PatEmpty)))

(proc AppRecvAuth ((p pat))
  (prefix (ChDispLogin AppAuthSuccess) (unwind AppDispSearch QueryEmpty p)))

(proc AppDispSearch ((q query) (p pat))
  (ex 
    (prefix EvLogoutBtn (unwind AppDispLogin PatEmpty))
    (prefixRecv (univ chSearchField) ch
      (match ch
        (ChSearchField q (unwind AppDispSearch q p))))
    (guard
      (not (eq query q QueryEmpty))
      (prefix EvSearchBtn (unwind AppDidPressSearchBtn q p)))))

(proc AppDidPressSearchBtn ((q query) (p pat))
  (prefix (ChSearchReq q Page1) (unwind AppReqSearch q p)))

(proc AppReqSearch ((q query) (p pat))
  (prefixRecv (univ chSearchRes) ch
    (match ch
      (ChSearchResError (unwind AppDialogSearchError q p))
      (ChSearchResOk repos hasMore (unwind AppRecvSearch repos hasMore q p)))))

(proc AppDialogSearchError ((q query) (p pat))
  (prefix ChDispSearchError (unwind AppDispSearch q p)))

(proc AppRecvSearch ((repos (list repo)) (hasMore bool) (q query) (p pat))
  (prefix
    (ChDispSearchOk repos hasMore q p)
    (unwind AppDispSearchResult repos hasMore q (empty (map repo (option bool))) Page1 p)))

(proc AppDispSearchResult ((repos (list repo)) (hasMore bool) (q query) (starMap (map repo (option bool))) (page page) (p pat))
  (ex
    (prefix EvMoreBtn (unwind AppDispPressMoreBtn repos hasMore q starMap page p))
    (prefixRecv
      ; Cannot check star if the repo was not fetched yet
      (filter (set chChkStarBtn)
        ch (match ch
          (ChChkStarBtn repo (contains (list repo) repo repos)))
        (univ chChkStarBtn))
      ch
      (match ch
        (ChChkStarBtn repo (unwind AppDidPressChkStar repo repos hasMore q starMap page p))))))

(proc AppDispPressMoreBtn ((repos (list repo)) (hasMore bool) (q query) (starMap (map repo (option bool))) (page page) (p pat))
  ; TODO
  stop)

(proc AppDidPressChkStar ((repo repo) (repos (list repo)) (hasMore bool) (q query) (starMap (map repo (option bool))) (page page) (p pat))
  ; TODO
  stop)
