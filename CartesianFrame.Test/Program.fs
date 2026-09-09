namespace CartesianFrame

module Program =

    let pairs =
        [
            (1, 1), 1
            (1, 2), 2
            (1, 3), 3
            (2, 1), 4
            (2, 2), 5
            (2, 3), 6
            (3, 1), 4
            (3, 2), 5
            (3, 3), 6
        ]
    let C = Tests.toFrame [1..3] [1..3] pairs
    let D = Tests.toFrame [1..2] [1..3] (List.take 6 pairs)
    let C' = CartesianFrame.collapse C
    CartesianFrame.print C'

