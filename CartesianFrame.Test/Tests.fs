namespace CartesianFrame

open System
open Xunit

type RoadOld = Seaside = 0 | Highway = 1
type RoadNew = Seaside = 10 | Highway = 11 | Country = 12
type WeatherOld = Rainy = 0 | Cloudy = 1 | Sunny = 2
type WeatherNew = Rainy = 10 | Cloudy = 11 | Sunny = 12

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

module Tests =

    [<Fact>]
    let driver () =
        let C =
            CartesianFrame.ofTuples [
                RoadOld.Seaside, WeatherOld.Rainy, 1
                RoadOld.Seaside, WeatherOld.Cloudy, 5
                RoadOld.Seaside, WeatherOld.Sunny, 7
                RoadOld.Highway, WeatherOld.Rainy, 5
                RoadOld.Highway, WeatherOld.Cloudy, 5
                RoadOld.Highway, WeatherOld.Sunny, 5
            ]
        let D =
            CartesianFrame.ofTuples [
                RoadNew.Seaside, WeatherNew.Rainy, 1
                RoadNew.Seaside, WeatherNew.Cloudy, 5
                RoadNew.Seaside, WeatherNew.Sunny, 7
                RoadNew.Highway, WeatherNew.Rainy, 5
                RoadNew.Highway, WeatherNew.Cloudy, 5
                RoadNew.Highway, WeatherNew.Sunny, 5
                RoadNew.Country, WeatherNew.Rainy, 6
                RoadNew.Country, WeatherNew.Cloudy, 6
                RoadNew.Country, WeatherNew.Sunny, 6
            ]
        let g = function
            | RoadOld.Seaside -> RoadNew.Seaside
            | RoadOld.Highway -> RoadNew.Highway
            | _ -> failwith "Unexpected"
        let h = function
            | WeatherNew.Rainy -> WeatherOld.Rainy
            | WeatherNew.Cloudy -> WeatherOld.Cloudy
            | WeatherNew.Sunny -> WeatherOld.Sunny
            | _ -> failwith "Unexpected"
        Assert.True(CartesianFrame.isMorphism C D (g, h))
