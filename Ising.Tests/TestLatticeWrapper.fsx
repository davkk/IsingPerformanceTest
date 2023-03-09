#r "nuget: Plotly.NET"
#r "nuget: Plotly.NET.ImageExport"

#load "../Ising/Domain.fs"
#load "../Ising/Jagged2DArray.fs"

open System.IO
open Plotly.NET
open Plotly.NET.ImageExport

open Domain
open Jagged2DArray

let parameters: Parameters =
    {
        Rng = System.Random()
        LatticeSize = 256
        Sweeps = 10_000_000
        NumOfStates = 2
        Beta = 1.4
    }


let lattice =
    Lattice(parameters.LatticeSize, parameters.Rng)

lattice |> Ising.simulate parameters


Chart.Heatmap(
    lattice.Spins
    |> Array.toList,
    ColorScale = StyleParam.Colorscale.Hot
)
|> Chart.withSize (1200, 600)
|> Chart.saveHtml (
    Path.Combine(__SOURCE_DIRECTORY__, "lattice-test-after.html")
)
