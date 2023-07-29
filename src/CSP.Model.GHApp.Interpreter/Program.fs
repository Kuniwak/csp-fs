open CSP.Core.CLI
open CSP.Model.GHApp

let natMax = 5u
let listMax = 3u
let cfg = interpreterConfig natMax listMax
let pn = "GHSearch" in

start cfg procMap ctorMap genv pn []