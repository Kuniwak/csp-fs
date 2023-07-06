module CSP.Core.Tests

open Xunit
open CSP.Core.Proc

[<Fact>]
let prefix () =
    let p: Proc<Unit, Unit, Unit, Unit> = Prefix((), Stop) in
    let m = Map.empty in
    let env = Map.empty in
    Assert.Equal<Event<Unit> list list>([ [ Vis() ] ], traces m env p 5)

[<Fact>]
let unwind () =
    let p: Proc<Unit, Unit, Unit, Unit> = Unwind() in
    let m = Map [ ((), Prefix((), p)) ] in
    let env = Map.empty in

    Assert.Equal<Event<Unit> list list>(
        [ [ Vis() ]
          [ Vis(); Vis() ]
          [ Vis(); Vis(); Vis() ]
          [ Vis(); Vis(); Vis(); Vis() ] ],
        traces m env p 4
    )

[<Fact>]
let aLoopBLoop () =
    let p0: Proc<int, int, Unit, Unit> = Prefix(0, Unwind 1) in
    let p1: Proc<int, int, Unit, Unit> = Prefix(1, Unwind 0) in
    let m = Map [ (0, p0); (1, p1) ] in
    let env = Map.empty in

    Assert.Equal<Event<int> list list>(
        [ [ Vis 0 ]
          [ Vis 0; Vis 1 ]
          [ Vis 0; Vis 1; Vis 0 ]
          [ Vis 0; Vis 1; Vis 0; Vis 1 ] ],
        traces m env p0 4
    )

[<Fact>]
let rand2 () =
    let p: Proc<Unit, int, Unit, Unit> = IntCh(Prefix(1, Unwind()), Prefix(2, Unwind())) in
    let m = Map [ ((), p) ] in
    let env = Map.empty in

    Assert.Equal<Event<int> list list>([ [ Tau ]; [ Tau ]; [ Tau; Vis 1 ]; [ Tau; Vis 2 ] ], traces m env p 2)

[<Fact>]
let seq () =
    let p: Proc<Unit, int, Unit, Unit> = Seq(Prefix(1, Skip), Prefix(2, Skip)) in
    let m = Map.empty in
    let env = Map.empty in

    Assert.Equal<Event<int> list list>(
        [ [ Vis 1 ]
          [ Vis 1; Tau ]
          [ Vis 1; Tau; Vis 2 ]
          [ Vis 1; Tau; Vis 2; Tick ] ],
        traces m env p 5
    )

[<Fact>]
let lr () =
    let l: Proc<int, int, Unit, Unit> = Prefix(1, Prefix(0, Unwind 0)) in
    let r: Proc<int, int, Unit, Unit> = Prefix(2, Prefix(0, Unwind 1)) in
    let p: Proc<int, int, Unit, Unit> = InterfaceParallel(l, Set [ 0 ], r) in
    let m = Map [ (0, l); (1, r); (2, p) ] in
    let env = Map.empty in
    Assert.Equal<Event<int> list list>([
        [Vis 1]
        [Vis 2]
        [Vis 1; Vis 2]
        [Vis 1; Vis 2; Vis 0]
        [Vis 2; Vis 1]
        [Vis 2; Vis 1; Vis 0]
    ], traces m env p 3)