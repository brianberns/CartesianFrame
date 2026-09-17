namespace CartesianFrame

open System
open System.Collections.Generic

/// Cartesian frame as defined in https://arxiv.org/pdf/2109.10996.
[<CustomEquality; NoComparison>]
type CartesianFrame<'Action, 'Environment, 'World
    when 'Action : comparison
    and 'Environment : comparison
    and 'World : equality> =
    {
        /// Actions available to the agent.
        Actions : Set<'Action>

        /// Possible environmental states.
        Environments : Set<'Environment>

        /// Answers the outcome of taking the given action in the
        /// given state.
        Operator : 'Action * 'Environment -> 'World
    }

    /// Answers the outcome of taking the given action in the
    /// given state.
    member frame.Item(a, e) =
        frame.Operator(a, e)

    /// Determines whether two frames are equal.
    member frame.Equals(other) =
        let comparer = EqualityComparer<'World>.Default   // e.g. NaN is equal to itself
        frame.Actions = other.Actions
            && frame.Environments = other.Environments
            && Seq.forall (fun (a, e) ->
                comparer.Equals(
                    frame[a, e],
                    other[a, e]))
                (Seq.allPairs
                    frame.Actions
                    frame.Environments)

    interface IEquatable<CartesianFrame<'Action, 'Environment, 'World>> with

        /// Determines whether two frames are equal.
        member frame.Equals(other) = frame.Equals(other)

    /// Determines whether two frames are equal.
    override frame.Equals(obj) =
        match obj with
            | :? CartesianFrame<'Action, 'Environment, 'World> as other ->
                frame.Equals(other)
            | _ -> false

    /// Hashes the given frame such that two equal frames produce
    /// the same result.
    override frame.GetHashCode() =
        let comparer = EqualityComparer<'World>.Default
        let hc = HashCode()
        hc.Add(frame.Actions)
        hc.Add(frame.Environments)
        for a in frame.Actions do
            for e in frame.Environments do
                hc.Add(frame[a, e], comparer)
        hc.ToHashCode()

module CartesianFrame =

    /// Creates a frame where the agent chooses a world
    /// directly.
    let ofWorlds worlds =
        {
            Actions = worlds
            Environments = Set.singleton ()
            Operator = fst
        }

    /// Creates a frame where the environment chooses a
    /// world directly.
    let one worlds =
        {
            Actions = Set.singleton ()
            Environments = worlds
            Operator = snd
        }

    /// Possible worlds produced by the given frame.
    let image C =
        set [
            for a in C.Actions do
                for e in C.Environments -> C[a, e]
        ]

    /// Answers the dual (matrix transposition) of the given frame.
    let dual C =
        {
            Actions = C.Environments
            Environments = C.Actions
            Operator = fun (e, a) -> C[a, e]
        }

    /// Collapses the rows of the given frame.
    let private collapseRows C =
        let actions =
            seq {
                for a in C.Actions do
                    let row =
                        [ for e in C.Environments -> C[a, e] ]
                    row, a
            }
                |> Seq.groupBy fst
                |> Seq.map (fun (_, group) ->
                    Seq.head group |> snd)   // arbitrarily choose an action to represent the entire group
                |> set
        {
            Actions = actions
            Environments = C.Environments
            Operator = C.Operator
        }

    /// Answers the "biextensional collapse" of the given frame
    /// by deleting duplicate rows and columns.
    let collapse C =
        C
            |> collapseRows
            |> dual
            |> collapseRows
            |> dual

    /// Maps the given function over the given frame.
    let map f C =
        {
            Actions = C.Actions
            Environments = C.Environments
            Operator = fun key -> C[key] |> f
        }

    /// Uses frame C to refine frame D's values.
    let apply C D =
        assert((image D).IsSubsetOf(C.Actions))
        {
            Actions = D.Actions
            Environments =
                set [
                    for ed in D.Environments do
                        for ec in C.Environments do
                            ed, ec
                ]
            Operator =
                fun (ad, (ed, ec)) ->
                    let ac = D[ad, ed]
                    C[ac, ec]
        }

    /// Assumes a subset of environments.
    let assume (envs : Set<_>) C =
        assert(envs.IsSubsetOf(C.Environments))
        {
            Actions = C.Actions
            Environments = envs
            Operator = C.Operator
        }

    /// Commits to a subset of actions.
    let commit (actions : Set<_>) C =
        assert(actions.IsSubsetOf(C.Actions))
        {
            Actions = actions
            Environments = C.Environments
            Operator = C.Operator
        }

    /// Permutes the given list.
    let rec permute = function
        | [] -> [ [] ]
        | xs ->
            [
                for x in xs do
                    for p in permute (List.except [ x ] xs) ->
                        x :: p
            ]

    /// Are the given frames equivalent? This is true iff their
    /// biextensional collapses are isomorphic.
    let areEquivalent C D =
        let C' = collapse C
        let D' = collapse D
        if C'.Actions.Count = D'.Actions.Count
            && C'.Environments.Count = D'.Environments.Count
            && image C' = image D' then
                let acs = Set.toList C'.Actions
                let ads = Set.toList D'.Actions
                Seq.exists (fun acs ->                    // is there a permutation of C's actions that matches D's actions?
                    let pairs = Seq.zip acs ads
                    Seq.forall (fun ed ->                 // do all of D's environments match one of C's?
                        Seq.exists (fun ec ->             // is there a C environment that matches this D environment?
                            Seq.forall (fun (ac, ad) ->   // do all of the action pairs have the same value in these two environments?
                                C'[ac, ec] = D'[ad, ed])
                                pairs)
                            C'.Environments)
                        D'.Environments)
                    (permute acs)
        else false

    /// Determines whther the given set of "blocks"
    /// partition the given set of items.
    let private isPartition blocks items =
        not (Set.contains Set.empty blocks)                   // no empty blocks
            && Set.unionMany blocks = items                   // blocks contain every item and no extra items
            && Seq.sumBy Set.count blocks = Set.count items   // blocks don't overlap

    /// Cartesian product of the given lists.
    let rec private product = function
        | [] -> [ [] ]
        | x :: xs ->
            let restProduct = product xs
            [ for item in x do
                for rest in restProduct ->
                    item :: rest ]

    /// Determines all possible choice functions for the
    /// given partition.
    let private getChoiceFunctions (partition : Set<Set<_>>) =
        let partition = Set.toList partition
        product partition
            |> Seq.map (
                Seq.zip partition >> Map)

    /// Moves block choice into the environment, retaining
    /// fine-grained action choice.
    let externalizeBlock partition C =
        assert(isPartition partition C.Actions)
        {
            Actions = set (getChoiceFunctions partition)
            Environments =
                set [
                    for block in partition do
                        for env in C.Environments ->
                            block, env
                ]
            Operator =
                fun (choiceFunc, (block, env)) ->
                    let action = choiceFunc[block]
                    C[action, env]
        }

    /// Moves fine-grained action choice into the environment,
    /// retaining block choice.
    let externalizeChoice partition C =
        assert(isPartition partition C.Actions)
        {
            Actions = partition
            Environments =
                set [
                    for choiceFunc in getChoiceFunctions partition do
                        for env in C.Environments ->
                            choiceFunc, env
                ]
            Operator =
                fun (block, (choiceFunc, env)) ->
                    let action = choiceFunc[block]
                    C[action, env]
        }

    /// Moves block choice into the agent, excluding
    /// fine-grained environment choice.
    let internalizeBlock partition C =
        dual (externalizeBlock partition (dual C))

    /// Moves fine-grained environment choice into the agent,
    /// excluding block choice.
    let internalizeChoice partition C =
        dual (externalizeChoice partition (dual C))
