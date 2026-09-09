namespace CartesianFrame

open System
open Xunit

type RoadOld = Seaside = 0 | Highway = 1
type RoadNew = Seaside = 10 | Highway = 11 | Country = 12
type WeatherOld = Rainy = 0 | Cloudy = 1 | Sunny = 2
type WeatherNew = Rainy = 10 | Cloudy = 11 | Sunny = 12

module Tests =

    let toFrame triples =
        let map =
            triples
                |> Seq.map (fun (a : 'A, e : 'E, w) ->
                    (a, e), w)
                |> Map
        {
            Actions = Enum.GetValues<'A>()
            Environment = Enum.GetValues<'E>()
            Operator = fun key -> map[key]
        }

    [<Fact>]
    let ``Driver`` () =
        let C =
            toFrame [
                RoadOld.Seaside, WeatherOld.Rainy, 1
                RoadOld.Seaside, WeatherOld.Cloudy, 5
                RoadOld.Seaside, WeatherOld.Sunny, 7
                RoadOld.Highway, WeatherOld.Rainy, 5
                RoadOld.Highway, WeatherOld.Cloudy, 5
                RoadOld.Highway, WeatherOld.Sunny, 5
            ]
        let D =
            toFrame [
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
