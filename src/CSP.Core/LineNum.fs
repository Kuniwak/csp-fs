module CSP.Core.LineNum

type LineNum = string

let ofNat (n: uint) : LineNum = $"%d{n}"
