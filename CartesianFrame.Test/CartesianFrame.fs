namespace CartesianFrame

open System

open FsCheck
open FsCheck.FSharp

module CartesianFrame =

    let ofTuples tuples =
        let actions =
            tuples
                |> Seq.map (fun (a, _, _) -> a)
                |> set
        let envs =
            tuples
                |> Seq.map (fun (_, e, _) -> e)
                |> set
        let map =
            tuples
                |> Seq.map (fun (a, e, w) -> (a, e), w)
                |> Map
        {
            Actions = actions
            Environments = envs
            Operator = fun key -> map[key]
        }

    /// Display string.
    let toString frame =
        seq {
            ""
            seq {
                ""
                for e in frame.Environments do
                    string e
            } |> String.concat "\t"

            for a in frame.Actions do
                seq {
                    string a
                    for e in frame.Environments do
                        string frame[a, e]
                } |> String.concat "\t"
        } |> String.concat Environment.NewLine
