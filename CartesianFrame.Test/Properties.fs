namespace CartesianFrame

open FsCheck.FSharp
open FsCheck.Xunit

module Properties =

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
            reassociate (apply C (apply D E)) =
                apply (apply C D) E)

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
                = C)

    [<Property>]
    let ``Dual is its own inverse`` (C : CartesianFrame<string, string, string>) =
        dual (dual C) = C

    [<Property>]
    let ``Hashes of equal frames are equal`` (C : CartesianFrame<string, string, int>) =
        hash (dual (dual C)) = hash C

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
                    // collapsed frame keeps the original cell values
                && C' = commit C'.Actions (assume C'.Environments C)
                && getRowsWithin C C' = getRows C
                && getColumnsWithin C C' = getColumns C)

    [<Property>]
    let ``Collapsed frame is biextensional`` () =
        Prop.forAll (Arb.fromGen genFrame) (fun C ->
            let C' = collapse C
            (getRows C').Count = C'.Actions.Count
                && (getColumns C').Count = C'.Environments.Count)

    [<Property>]
    let ``Collapse and dual commute`` () =
        Prop.forAll (Arb.fromGen genFrame) (fun C ->
            collapse (dual C) =
                dual (collapse C))

    [<Property>]
    let ``Row vector and column vector are duals`` (S : Set<int>) =
        dual (ofWorlds S) = one S

    [<Property>]
    let ``Map preserves identity`` (C : CartesianFrame<string, string, int>) =
        map id C = C

    [<Property>]
    let ``Map preserves composition``
        (f : int -> float)
        (g : float -> string)
        (C : CartesianFrame<string, string, int>) =
        map g (map f C) = map (f >> g) C

    [<Property>]
    let ``Assume and commit are duals``
        (C : CartesianFrame<string, string, int>) =

        let genSubset =
            gen {
                let! env = Gen.subListOf C.Environments
                return set env
            }

        Prop.forAll (Arb.fromGen genSubset) (fun envs ->
            assume envs C =
                dual (commit envs (dual C)))

    [<Property>]
    let ``Commit is apply with a subset of actions`` () =

        let fix C =
            {
                Actions = C.Actions
                Environments =
                    Set.map snd C.Environments
                Operator =
                    fun (a, e) -> C[a, ((), e)]
            }

        let genCase =
            gen {
                let! C = genFrame
                let! actions = Gen.subListOf C.Actions
                return C, set actions
            }

        Prop.forAll (Arb.fromGen genCase) (fun (C, actions) ->
            commit actions C =
                fix (apply C (ofWorlds actions)))

    /// Disguises the given frame without changing the decision
    /// problem it represents: renames actions and environments
    /// to strings in a random order, and sometimes duplicates a
    /// row and/or a column. The result is always equivalent to
    /// the given frame.
    let private genDisguise (C : CartesianFrame<int, int, int>) =
        gen {
            let! actionOrder = Gen.shuffle C.Actions
            let! envOrder = Gen.shuffle C.Environments
            let! dupAction =
                if C.Actions.IsEmpty then Gen.constant None
                else Gen.elements C.Actions |> Gen.optionOf
            let! dupEnv =
                if C.Environments.IsEmpty then Gen.constant None
                else Gen.elements C.Environments |> Gen.optionOf

                // new label -> original label
            let actionOf =
                Map [
                    yield! actionOrder |> Seq.mapi (fun i a -> $"a{i}", a)
                    yield! dupAction |> Option.map (fun a -> "a-dup", a) |> Option.toList
                ]
            let envOf =
                Map [
                    yield! envOrder |> Seq.mapi (fun i e -> $"e{i}", e)
                    yield! dupEnv |> Option.map (fun e -> "e-dup", e) |> Option.toList
                ]

            return {
                Actions = set actionOf.Keys
                Environments = set envOf.Keys
                Operator = fun (a, e) -> C[actionOf[a], envOf[e]]
            }
        }

    [<Property>]
    let ``Disguised copies are equivalent`` () =
        let genCase =
            gen {
                let! C = genFrame
                let! D = genDisguise C
                return C, D
            }
        Prop.forAll (Arb.fromGen genCase) (fun (C, D) ->
            areEquivalent C D)

    /// Slow but obviously correct equivalence check: tries every
    /// pairing of actions and every pairing of environments of the
    /// collapsed frames. Only practical for tiny frames.
    let private bruteForceEquivalent C D =
        let C' = collapse C
        let D' = collapse D
        let ads = Set.toList D'.Actions
        let eds = Set.toList D'.Environments
        C'.Actions.Count = D'.Actions.Count
            && C'.Environments.Count = D'.Environments.Count
            && permute (Set.toList C'.Actions)
                |> Seq.exists (fun acs ->
                    permute (Set.toList C'.Environments)
                        |> Seq.exists (fun ecs ->
                            Seq.forall2 (fun ac ad ->
                                Seq.forall2 (fun ec ed ->
                                    C'[ac, ec] = D'[ad, ed])
                                    ecs eds)
                                acs ads))

    /// A pair of frames that are equivalent about half the time:
    /// the second is a disguised copy of either the first frame
    /// or an independent one.
    let private genPair =
        gen {
            let! C = genFrame
            let! source = Gen.oneof [ Gen.constant C; genFrame ]
            let! D = genDisguise source
            return C, D
        }

    [<Property>]
    let ``Equivalence agrees with brute force`` () =
        Prop.forAll (Arb.fromGen genPair) (fun (C, D) ->
            areEquivalent C D = bruteForceEquivalent C D)

    /// A frame, plus a random partition of its actions.
    let private genPartitioned =
        gen {
            let! C = genFrame
            let! blockOf =
                Gen.choose (0, max 0 (C.Actions.Count - 1))
                    |> Gen.listOfLength C.Actions.Count
            let partition =
                Seq.zip C.Actions blockOf
                    |> Seq.groupBy snd
                    |> Seq.map (snd >> Seq.map fst >> set)
                    |> set
            return C, partition
        }

    [<Property>]
    let ``Choice functions are valid and complete`` () =
        Prop.forAll (Arb.fromGen genPartitioned) (fun (C, partition) ->
            let choiceFuncs = (externalizeBlock partition C).Actions
            let isValid (choiceFunc : Map<Set<int>, int>) =
                set choiceFunc.Keys = partition
                    && partition |> Set.forall (fun block ->
                        block.Contains(choiceFunc[block]))
            Set.forall isValid choiceFuncs
                    // a set of valid choice functions this large must contain all of them
                && choiceFuncs.Count = (partition |> Seq.map Set.count |> Seq.fold ( * ) 1))

    [<Property>]
    let ``Re-bracketing an externalized block gives back the frame`` () =
        Prop.forAll (Arb.fromGen genPartitioned) (fun (C, partition) ->
            let X = externalizeBlock partition C
                // move the block from the environment back into the agent (Claim 45)
            let team =
                {
                    Actions =
                        set [
                            for choiceFunc in X.Actions do
                                for block in partition ->
                                    choiceFunc, block
                        ]
                    Environments = C.Environments
                    Operator =
                        fun ((choiceFunc, block), env) ->
                            X[choiceFunc, (block, env)]
                }
            areEquivalent team C)

    [<Property>]
    let ``Both externalizations encode the same outcomes`` () =
        Prop.forAll (Arb.fromGen genPartitioned) (fun (C, partition) ->
            let X = externalizeBlock partition C
            let Y = externalizeChoice partition C
            X.Environments =
                set [
                    for block in partition do
                        for env in C.Environments -> block, env
                ]
                && Y.Actions = partition
                && Y.Environments =
                    set [
                        for choiceFunc in X.Actions do
                            for env in C.Environments -> choiceFunc, env
                    ]
                && Seq.forall (fun (block, choiceFunc, env) ->
                        Y[block, (choiceFunc, env)] = X[choiceFunc, (block, env)])
                    (seq {
                        for block in partition do
                            for choiceFunc in X.Actions do
                                for env in C.Environments -> block, choiceFunc, env
                    }))

    [<assembly: Properties(
        Verbose = false)>]
    do ()
