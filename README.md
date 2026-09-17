# Cartesian Frames

An F# implementation of [Cartesian Frames](https://arxiv.org/pdf/2109.10996).

Cartesian frames provide a framework for agents to choose from a set of available actions in an environment with multiple possible states.

For example, consider a driver who is deciding whether to drive by the seaside vs. the highway. There are three possible weather conditions: rainy, cloudy, and sunny. We represent her decision situation with the following Cartesian frame, where the values are numbers representing how much she likes each outcome:

|         | Rainy | Cloudy | Sunny |
| ------- | ----: | -----: | ----: |
| Seaside |     1 |      5 |     7 |
| Highway |     5 |      5 |     5 |

Using this library, this can be written in F# as:

```fsharp
{
    Actions = set [ "Seaside"; "Highway" ]
    Environments = set [ "Rainy"; "Cloudy"; "Sunny" ]
    Operator = function
        | "Seaside", "Rainy" -> 1
        | "Seaside", "Cloudy" -> 5
        | "Seaside", "Sunny" -> 7
        | "Highway", "Rainy" -> 5
        | "Highway", "Cloudy" -> 5
        | "Highway", "Sunny" -> 5
}
```
