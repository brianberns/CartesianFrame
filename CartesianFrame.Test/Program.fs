namespace CartesianFrame

module Program =

    do
        let pairs =
            [
                "a1", "e1", "w1"
                "a1", "e2", "w2"
                "a1", "e3", "w3"
                "a2", "e1", "w4"
                "a2", "e2", "w5"
                "a2", "e3", "w6"
            ]
        let C =
            CartesianFrame.ofTuples
                [ "a1"; "a2" ]
                [ "e1"; "e2"; "e3" ]
                pairs
        CartesianFrame.print C

    do
        let pairs =
            [
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
        let D =
            CartesianFrame.ofTuples
                [ "b1"; "b2"; "b3" ]
                [ "f1"; "f2"; "f3" ]
                pairs
        CartesianFrame.print D
        CartesianFrame.print (CartesianFrame.collapse D)
