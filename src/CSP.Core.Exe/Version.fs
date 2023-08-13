module CSP.Core.Exe.Version

open System.Reflection

let version = Assembly.GetExecutingAssembly().GetName().Version.ToString()

