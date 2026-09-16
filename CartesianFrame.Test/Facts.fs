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
        Assert.True(
            CartesianFrame.areEqual expected actual)
