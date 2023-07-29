open CSP.Core.Eval
open CSP.Core.Univ
open CSP.Core.Search
open CSP.Core.Visualization.DotLang
open CSP.Model.GHApp

let nodeMax = 10000

let natMax = 5u
let listMax = 3u
let cfg = dotConfig (searchConfig 10000) (evalConfig (univConfig natMax listMax)) 

printfn $"%s{dot cfg procMap ctorMap genv ("GHSearch") []}"
