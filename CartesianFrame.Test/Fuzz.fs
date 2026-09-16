namespace CartesianFrame

open FsCheck.FSharp
open FsCheck.Xunit

module Fuzz =

    open CartesianFrame

    /// Map-backed frame: actions {0..nA-1}, envs {0..nE-1}, worlds from `worlds`.
    let private genFrameImpl nA nE worlds =
        gen {
            let keys =
                [
                    for a in 0 .. nA - 1 do
                        for e in 0 .. nE - 1 -> a, e
                ]
            let! values =
                Gen.elements worlds
                    |> Gen.listOfLength keys.Length
            let map = List.zip keys values |> Map
            return {
                Actions = set [ 0 .. nA - 1 ]
                Environments = set [ 0 .. nE - 1 ]
                Operator = fun key -> map[key]
            }
        }

    let private genFrame =
        gen {
            let genSize =
                Gen.frequency [
                    1, Gen.constant 0
                    9, Gen.choose (1, 3)
                ]
            let! nA = genSize
            let! nE = genSize
            let! nW =
                if nA > 0 && nE > 0 then Gen.choose (1, 4)
                else Gen.constant 0
            return! genFrameImpl nA nE [ 0 .. nW - 1 ]
        }

    [<Property>]
    let ``Functor composition`` () =

        /// Re-brackets a composite environment.
        let reassociate C =
            {
                Actions = C.Actions
                Environments =
                    C.Environments
                        |> Set.map (fun ((x, y), z) ->
                            x, (y, z))
                Operator =
                    fun (a, (x, (y, z))) -> C[a, ((x, y), z)]
            }

        /// (C, D, E) with D over Agent(C) and E over Agent(D).
        let genTower =
            gen {
                let! nW = Gen.choose (1, 4)
                let! nC = Gen.choose (1, 3)
                let! mC = Gen.choose (1, 3)
                let! nD = Gen.choose (1, 3)
                let! mD = Gen.choose (1, 3)
                let! nE = Gen.choose (1, 3)
                let! mE = Gen.choose (1, 3)
                let! C = genFrameImpl nC mC [ 0 .. nW - 1 ]   // worlds from W
                let! D = genFrameImpl nD mD [ 0 .. nC - 1 ]   // worlds from Agent(C)
                let! E = genFrameImpl nE mE [ 0 .. nD - 1 ]   // worlds from Agent(D)
                return C, D, E
            }

        Prop.forAll (Arb.fromGen genTower) (fun (C, D, E) ->
            areEqual
                (reassociate (apply C (apply D E)))
                (apply (apply C D) E))

    [<Property>]
    let ``Left unit law`` () =

        let fix C =
            {
                Actions = C.Actions
                Environments =
                    Set.map fst C.Environments
                Operator =
                    fun (a, e) -> C[a, (e, ())]
            }

        Prop.forAll (Arb.fromGen genFrame) (fun C ->
            apply (ofWorlds (image C)) C
                |> fix
                |> areEqual C)

    [<Property>]
    let ``Right unit law`` () =

        let fix C =
            {
                Actions = C.Actions
                Environments =
                    Set.map snd C.Environments
                Operator =
                    fun (a, e) -> C[a, ((), e)]
            }

        Prop.forAll (Arb.fromGen genFrame) (fun C ->
            apply C (ofWorlds C.Actions)
                |> fix
                |> areEqual C)

    [<Property>]
    let ``Dual is its own inverse`` (C : CartesianFrame<string, string, string>) =
        areEqual
            (dual (dual C))
            C

    let private getRowsWithin C C' =
        set [
            for a in C'.Actions do
                [ for e in C.Environments -> C[a, e] ]
        ]

    let private getColumnsWithin C C' =
        set [
            for e in C'.Environments do
                [ for a in C.Actions -> C[a, e] ]
        ]

    let private getRows C = getRowsWithin C C

    let private getColumns C = getColumnsWithin C C

    [<Property>]
    let ``Collapse is nondestructive`` () =
        Prop.forAll (Arb.fromGen genFrame) (fun C ->
            let C' = collapse C
            C'.Actions.IsSubsetOf(C.Actions)
                && C'.Environments.IsSubsetOf(C.Environments)
                && getRowsWithin C C' = getRows C
                && getColumnsWithin C C' = getColumns C)

    [<Property>]
    let ``Collapsed frame is biextensional`` () =
        Prop.forAll (Arb.fromGen genFrame) (fun C ->
            let C' = collapse C
            (getRows C').Count = C'.Actions.Count
                && (getColumns C').Count = C'.Environments.Count)

    [<Property>]
    let ``Collapse is idempotent`` () =
        Prop.forAll (Arb.fromGen genFrame) (fun C ->
            areEqual
                (collapse (collapse C))
                (collapse C))

    [<Property>]
    let ``Collapse and dual commute`` () =
        Prop.forAll (Arb.fromGen genFrame) (fun C ->
            areEqual
                (collapse (dual C))
                (dual (collapse C)))

    [<Property>]
    let ``Row vector and column vector are duals`` (S : Set<int>) =
        areEqual
            (dual (ofWorlds S))
            (one S)

    [<Property>]
    let ``Map preserves identity`` (C : CartesianFrame<string, string, int>) =
        areEqual
            (map id C)
            C

    [<Property>]
    let ``Map preserves composition``
        (f : int -> float)
        (g : float -> string)
        (C : CartesianFrame<string, string, int>) =
        areEqual
            (map g (map f C))
            (map (f >> g) C)

    [<Property>]
    let ``Assume and commit are duals``
        (C : CartesianFrame<string, string, int>) =

        let genSubset =
            gen {
                let! env = Gen.subListOf C.Environments
                return set env
            }

        Prop.forAll (Arb.fromGen genSubset) (fun envs ->
            areEqual
                (assume envs C)
                (dual (commit envs (dual C))))

    [<assembly: Properties(
        Verbose = false)>]
    do ()
