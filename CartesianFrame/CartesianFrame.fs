namespace CartesianFrame

type CartesianFrame<'A, 'E, 'W> =
    {
        Actions : seq<'A>
        Environment : seq<'E>
        Operator : 'A * 'E -> 'W
    }

    member frame.Item(a, e) =
        frame.Operator(a, e)

module CartesianFrame =

    let isMorphism C D (g, h) =
        seq {
            for a in C.Actions do
                for f in D.Environment do
                    a, f
        } |> Seq.forall (fun (a, f) ->
            C[a, h f] = D[g a, f])
