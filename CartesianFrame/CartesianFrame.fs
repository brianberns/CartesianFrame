namespace CartesianFrame

open System

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
        if frame.Actions = other.Actions
            && frame.Environments = other.Environments then
            let pairs =
                seq {
                    for a in frame.Actions do
                        for e in frame.Environments ->
                            frame[a, e], other[a, e]
                }
            Seq.forall (fun (x, y) -> x = y) pairs
        else false

    interface IEquatable<CartesianFrame<'Action, 'Environment, 'World>> with

        /// Determines whether two frames are equal.
        member frame.Equals(other) = frame.Equals(other)

    /// Determines whether two frames are equal.
    override frame.Equals(obj : obj) =
        match obj with
        | :? CartesianFrame<'Action, 'Environment, 'World> as other -> frame.Equals(other)
        | _ -> false

    /// Hashes the given frame such that two equal frames produce
    /// the same result.
    override frame.GetHashCode() =
        hash (frame.Actions, frame.Environments)

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

    /// Applies the functor induced by frame C to frame D.
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
