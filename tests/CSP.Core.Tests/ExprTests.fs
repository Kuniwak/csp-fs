module CSP.Core.Tests.ExprTests

open Xunit
open CSP.Core.Val
open CSP.Core.Expr

[<Fact>]
let rec testDescendant () =
    let actual =
        descendant (If(Eq(Lit(VNat 0u), VarRef "x"), Plus(VarRef("x"), Lit(VNat 0u)), Minus(VarRef("x"), Lit(VNat 1u))))

    let expected =
        [ If(Eq(Lit(VNat 0u), VarRef "x"), Plus(VarRef("x"), Lit(VNat 0u)), Minus(VarRef("x"), Lit(VNat 1u)))
          Eq(Lit(VNat 0u), VarRef "x")
          Lit(VNat 0u)
          VarRef "x"
          Plus(VarRef("x"), Lit(VNat 0u))
          VarRef("x")
          Lit(VNat 0u)
          Minus(VarRef("x"), Lit(VNat 1u))
          VarRef("x")
          Lit(VNat 1u) ]

    Assert.Equal<Expr<string, Unit>>(expected, actual)
