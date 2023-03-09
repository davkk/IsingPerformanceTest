module Tests

open Xunit

open Domain

let parameters: Parameters =
    {
        Rng = System.Random(2001)
        Sweeps = 100_000
        LatticeSize = 64
        NumOfStates = 2
        Beta = 1.4
    }

let initialResult =
    // {|
    //     Beta = 1.4
    //     AvgE = -1.7097875681152344
    //     C = 0.45828185450241804
    //     AvgM = 0.046437027099609375
    //     X = 0.022453254845268566
    // |}
    let lattice = Initial.Ising.initLattice parameters
    Initial.Ising.simulate parameters lattice

[<Fact>]
let ``Compare Version1 with Initial`` () =
    let lattice =
        Version1.Ising.initLattice parameters.LatticeSize parameters.Rng

    let result=
        lattice
        |> Version1.Ising.simulate parameters

    System.Console.WriteLine initialResult
    System.Console.WriteLine result

    Assert.True(initialResult.AvgE = result.AvgE)
