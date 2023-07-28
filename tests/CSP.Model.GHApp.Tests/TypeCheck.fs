module CSP.Model.GHApp.TypeCheck

open Xunit
open CSP.Core
open CSP.Core.ProcMapTypeInference

[<Fact>]
let typeCheckGHApp () =
    match typeCheck ctorMap genv procMap with
    | None -> ()
    | Some(terr) ->
        Assert.Fail(TypeError.format terr)
    