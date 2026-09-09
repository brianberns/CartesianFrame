namespace CartesianFrame

type CartesianFrame<'A, 'E, 'W
    when 'A : comparison
    and 'E : comparison> =
    {
        Actions : Set<'A>
        Environments : Set<'E>
        Operator : 'A * 'E -> 'W
    }

    member frame.Item(a, e) =
        frame.Operator(a, e)

module Function =

    let isInjective f inputs =
        inputs
            |> Seq.countBy f
            |> Seq.forall (fun (_, count) ->
                count = 1)

    let isSurjective f inputs outputs =
        let image =
            Seq.map f inputs
                |> set
        Seq.forall (fun output ->
            image.Contains(output))
            outputs

    let isBijective f inputs outputs =
        isInjective f inputs
            && isSurjective f inputs outputs

module CartesianFrame =

    let isMorphism C D (g, h) =
        seq {
            for a in C.Actions do
                for f in D.Environments do
                    a, f
        } |> Seq.forall (fun (a, f) ->
            C[a, h f] = D[g a, f])

    let isIsomorphism C D (g, h) =
        isMorphism C D (g, h)
            && Function.isBijective g C.Actions C.Environments
            && Function.isBijective h D.Actions D.Environments

    let dual C =
        {
            Actions = C.Environments
            Environments = C.Actions
            Operator = fun (e, a) -> C[a, e]
        }

    let private collapseRows<'A, 'E, 'W
        when 'A : comparison
        and 'E : comparison
        and 'W : comparison> (C : CartesianFrame<'A, 'E, 'W>) =
        let actions =
            seq {
                for a in C.Actions do
                    let row =
                        [ for e in C.Environments -> C[a, e] ]
                    row, a
            }
                |> Seq.groupBy fst
                |> Seq.map (fun (_, group) ->
                    Seq.head group |> snd)
                |> set
        {
            Actions = actions
            Environments = C.Environments
            Operator = C.Operator
        }

    let collapse C =
        C
            |> collapseRows
            |> dual
            |> collapseRows
            |> dual

    let map f C =
        {
            Actions = C.Actions
            Environments = C.Environments
            Operator = fun key -> C[key] |> f
        }
