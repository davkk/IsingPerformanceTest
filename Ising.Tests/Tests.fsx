#r "nuget: Tensor"
#r "nuget: FSharp.Stats"
#r "nuget: Plotly.NET"
#r "nuget: Plotly.NET.ImageExport"

#load "../Ising/Domain.fs"
#load "../Ising/Initial.fs"
#load "../Ising/Version4.fs"

open System.IO
open Plotly.NET
open Plotly.NET.ImageExport

open Tensor
open FSharp.Stats

open Domain

let parameters: Parameters =
    {
        Rng = System.Random(2001)
        Sweeps = 100_000
        LatticeSize = 64
        NumOfStates = 2
        Beta = 1.4
    }


let initialLattice =
    Initial.Ising.initLattice parameters

let lattice =
    initialLattice
    |> Tensor.flatten
    |> HostTensor.toArray
    |> Array.map sbyte

[
    initialLattice
    |> HostTensor.map sbyte
    |> HostTensor.toList2D

    lattice
    |> Array.toList
    |> List.chunkBySize (int parameters.LatticeSize)
]
|> Seq.map (fun lat ->
    Chart.Heatmap(lat, ColorScale = StyleParam.Colorscale.Hot)
)
|> Chart.Grid(1, 2)
|> Chart.withSize (1200, 600)
|> Chart.saveHtml (Path.Combine(__SOURCE_DIRECTORY__, "lattice-test.html"))

let initialResult =
    Initial.Ising.simulate parameters initialLattice

let result =
    lattice
    |> Version4.Ising.simulate parameters

System.Console.WriteLine initialResult
System.Console.WriteLine result

[
    initialLattice |> HostTensor.map sbyte |> HostTensor.toList2D

    lattice
    |> Array.toList
    |> List.chunkBySize (int parameters.LatticeSize)
]
|> Seq.map (fun lat ->
    Chart.Heatmap(lat, ColorScale = StyleParam.Colorscale.Hot)
)
|> Chart.Grid(1, 2)
|> Chart.withSize (1200, 600)
|> Chart.saveHtml (
    Path.Combine(__SOURCE_DIRECTORY__, "lattice-test-after.html")
)
