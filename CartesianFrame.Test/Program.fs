namespace CartesianFrame

module Program =

    let example1 () =

        let C =
            CartesianFrame.ofTuples [
                "a1", "e1", "w1"
                "a1", "e2", "w2"
                "a1", "e3", "w3"
                "a2", "e1", "w4"
                "a2", "e2", "w5"
                "a2", "e3", "w6"
            ]
        printfn $"{C}"

        let D =
            CartesianFrame.ofTuples [
                "b1", "f1", "w1"
                "b1", "f2", "w2"
                "b1", "f3", "w3"
                "b2", "f1", "w4"
                "b2", "f2", "w5"
                "b2", "f3", "w6"
                "b3", "f1", "w4"
                "b3", "f2", "w5"
                "b3", "f3", "w6"
            ]
        printfn $"{D}"
        printfn $"{CartesianFrame.collapse D}"

    let example2 () =

        let D =
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
        printfn $"{D}"

        let D' =
            D
                |> CartesianFrame.map (function
                    | "o" | "a" -> 2
                    | "c" -> 3
                    | _ -> failwith "Unexpected")
                |> CartesianFrame.collapse
        printfn $"{D'}"

    let example3 () =

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

        let D' = CartesianFrame.apply C D
        printfn $"{D'}"

    example1 ()
    example2 ()
    example3 ()
