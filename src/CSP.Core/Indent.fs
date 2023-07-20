module CSP.Core.Indent

open System.Text.RegularExpressions
open FSharpPlus

type IndentDepth = uint
let indentWidth = 4u

let render (d: IndentDepth) : string =
    String.replicate (int (d * indentWidth)) " "

let onlinePattern = Regex("\n *", RegexOptions.Compiled)
let oneline (s: string) : string = onlinePattern.Replace(s, " ")
