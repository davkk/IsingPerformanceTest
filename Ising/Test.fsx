#r "nuget: Tensor"
#r "nuget: FSharp.Stats"
#r "nuget: FSharp.Json"

#load "./Domain.fs"
#load "./Initial.fs"

open System.IO
open Tensor
open FSharp.Json

open Domain
open Initial

let parameters: Parameters =
    {
        Rng = System.Random(2001)
        Sweeps = 1_000_000
        LatticeSize = 128
        NumOfStates = 2
        Beta = 1.4
    }

let lattice = Initial.initLattice parameters

let result =
    lattice |> Initial.simulate parameters

let finalLattice =
    lattice
    |> Tensor.flatten
    |> HostTensor.toArray

let data =
    {|
        result = result
        lattice = finalLattice
    |}

let json = Json.serialize data
File.WriteAllTextAsync (@"./results.json", json)
