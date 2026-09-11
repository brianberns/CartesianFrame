namespace CartesianFrame

open FsCheck
open FsCheck.FSharp

module CartesianFrame =

    let print C =

        printfn ""
        let headers =
            seq {
                ""
                for e in C.Environments do
                    string e
            } |> String.concat "\t"
        printfn $"{headers}"

        for a in C.Actions do
            let values =
                seq {
                    string a
                    for e in C.Environments do
                        string C[a, e]
                } |> String.concat "\t"
            printfn $"{values}"

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
