namespace CartesianFrame

open FsCheck.FSharp
open FsCheck.Xunit

type TestFrame = CartesianFrame<int, int, int>

module Fuzz =

    open CartesianFrame

    /// Re-brackets a composite environment.
    let private reassociate C =
        {
            Actions = C.Actions
            Environments =
                C.Environments
                    |> Set.map (fun ((x, y), z) ->
                        x, (y, z))
            Operator =
                fun (a, (x, (y, z))) -> C[a, ((x, y), z)]
        }

    [<Property>]
    let ``Functor composition`` () =

        /// Map-backed frame: actions {0..nA-1}, envs {0..nE-1}, worlds from `worlds`.
        let genFrame nA nE worlds =
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

        /// (C, D, E) with D over Agent(C) and E over Agent(D).
        let genTower =
            gen {
                let! nW = Gen.choose (1, 4)
                let! nV = Gen.choose (1, 3)
                let! nU = Gen.choose (1, 3)
                let! nB = Gen.choose (1, 3)
                let! mC = Gen.choose (1, 3)
                let! mD = Gen.choose (1, 3)
                let! mE = Gen.choose (1, 3)
                let! C = genFrame nV mC [ 0 .. nW - 1 ]   // worlds from W
                let! D = genFrame nU mD [ 0 .. nV - 1 ]   // worlds from Agent(C)
                let! E = genFrame nB mE [ 0 .. nU - 1 ]   // worlds from Agent(D)
                return C, D, E
            }

        Prop.forAll (Arb.fromGen genTower) (fun (C, D, E) ->
            areEqual
                (reassociate (apply C (apply D E)))
                (apply (apply C D) E))

    [<assembly: Properties(
        Verbose = false)>]
    do ()
