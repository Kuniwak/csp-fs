module CSP.Core.Tests.ExprTests

open Xunit
open CSP.Core.Val
open CSP.Core.Expr

[<Fact>]
let rec testDescendant () =
    let actual =
        descendant (If(NatEq(Lit(VNat 0u), VarRef "x"), NatAdd(VarRef("x"), Lit(VNat 0u)), NatSub(VarRef("x"), Lit(VNat 1u))))

    let expected =
        [ If(NatEq(Lit(VNat 0u), VarRef "x"), NatAdd(VarRef("x"), Lit(VNat 0u)), NatSub(VarRef("x"), Lit(VNat 1u)))
          NatEq(Lit(VNat 0u), VarRef "x")
          Lit(VNat 0u)
          VarRef "x"
          NatAdd(VarRef("x"), Lit(VNat 0u))
          VarRef("x")
          Lit(VNat 0u)
          NatSub(VarRef("x"), Lit(VNat 1u))
          VarRef("x")
          Lit(VNat 1u) ]

    Assert.Equal<Expr<string, Unit>>(expected, actual)
