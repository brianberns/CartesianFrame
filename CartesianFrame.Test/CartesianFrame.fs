namespace CartesianFrame

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
