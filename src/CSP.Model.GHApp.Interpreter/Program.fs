open CSP.Core.CLI
open CSP.Model.GHApp

let nodeMax = 10000
let natMax = 5u
let listMax = 3u
let cfg = dotConfig (searchConfig 10000) (evalConfig (univConfig natMax listMax)) 
let pn = "GHSearch" in

start 
