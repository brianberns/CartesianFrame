namespace CartesianFrame

open Xunit

module Facts =

    [<Fact>]
    let ``Collapse mapped values`` () =

        let actual =
            CartesianFrame.ofTuples [
                "C", "O", "o"
                "C", "A", "a"
                "C", "OC", "c"
                "C", "AC", "c"
                "~C", "O", "o"
                "~C", "A", "a"
                "~C", "OC", "o"
                "~C", "AC", "a"
            ]
            |> CartesianFrame.map (function
                | "o" | "a" -> 2
                | "c" -> 3
                | _ -> failwith "Unexpected")
            |> CartesianFrame.collapse

        let expected =
            CartesianFrame.ofTuples [
                "C", "A", 2
                "C", "AC", 3
                "~C", "A", 2
                "~C", "AC", 2
            ]

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Apply refines values`` () =

        let D =
            CartesianFrame.ofTuples [
                "Accept", "CA offer", "CA"
                "Decline", "CA offer", "WA"
                "Accept", "WA offer", "WA"
                "Decline", "WA offer", "WA"
            ]
        let C =
            CartesianFrame.ofTuples [
                "CA", "Capital", "Sacramento"
                "CA", "Largest", "Los Angeles"
                "WA", "Capital", "Olympia"
                "WA", "Largest", "Seattle"
            ]
        let actual = CartesianFrame.apply C D

        let expected =
            CartesianFrame.ofTuples [
                "Accept",  ("CA offer", "Capital"), "Sacramento"
                "Accept",  ("CA offer", "Largest"), "Los Angeles"
                "Accept",  ("WA offer", "Capital"), "Olympia"
                "Accept",  ("WA offer", "Largest"), "Seattle"
                "Decline", ("CA offer", "Capital"), "Olympia"
                "Decline", ("CA offer", "Largest"), "Seattle"
                "Decline", ("WA offer", "Capital"), "Olympia"
                "Decline", ("WA offer", "Largest"), "Seattle"
            ]

        Assert.Equal(expected, actual)

    [<Fact>]
    let ``Same row values, not equivalent`` () =

        let C =
            CartesianFrame.ofTuples [
                "CA", "Capital", "Sacramento"
                "CA", "Largest", "Los Angeles"
                "WA", "Capital", "Olympia"
                "WA", "Largest", "Seattle"
            ]

            // same row values up to order and same image, but no relabelling makes the frames match
        let D =
            CartesianFrame.ofTuples [
                "CA", "Capital", "Sacramento"
                "CA", "Largest", "Los Angeles"
                "WA", "Capital", "Seattle"
                "WA", "Largest", "Olympia"
            ]

        Assert.False(CartesianFrame.areEquivalent C D)
