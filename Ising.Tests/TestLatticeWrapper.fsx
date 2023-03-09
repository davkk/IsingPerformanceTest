#r "nuget: Plotly.NET"
#r "nuget: Plotly.NET.ImageExport"
#r "nuget: XoshiroPRNG.Net"

#load "../Ising/Domain.fs"
#load "../Ising/XorshiftRandom.fs"

open System.IO
open Plotly.NET
open Plotly.NET.ImageExport

open Xoshiro.PRNG64

open Domain
open XorshiftRandom

// let parameters: Parameters =
//     {
//         Rng = System.Random()
//         LatticeSize = 256
//         Sweeps = 10_000_000
//         NumOfStates = 2
//         Beta = 1.4
//     }

let parameters: SimParams =
    {
        Rng = XoRoShiRo128plus(2001)
        LatticeSize = 256
        Sweeps = 10_000_000
        Beta = 1.4
    }


let lattice =
    Lattice(parameters)

lattice |> Ising.simulate parameters


Chart.Heatmap(
    lattice.Spins
    |> Array.chunkBySize lattice.Size
    |> Array.toList,
    ColorScale = StyleParam.Colorscale.Hot
)
|> Chart.withSize (1200, 600)
|> Chart.saveHtml (
    Path.Combine(__SOURCE_DIRECTORY__, "lattice-test-after.html")
)
