namespace SbyteArray

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b

    let inline randomIndex max (rng: System.Random) =
        rng.Next(0, max), rng.Next(0, max)

    let inline idx (i: int) (j: int) (size: int) = i + j * size

    let inline spinSum (i, j) (lattice: array<sbyte>) size =
        lattice.[idx ((i - 1) %/ size) j size]
        + lattice.[idx ((i + 1) %/ size) j size]
        + lattice.[idx i ((j - 1) %/ size) size]
        + lattice.[idx i ((j + 1) %/ size) size]

open Helpers

module Ising =

    let initLattice (size: int) (rng: System.Random) =
        (fun _ -> if rng.NextDouble() < 0.5 then 1y else -1y)
        |> Array.init (size * size)

    let totalEnergy (lattice: array<sbyte>) size =
        let mutable sum = 0

        for i in 0 .. size - 1 do
            for j in 0 .. size - 1 do
                sum <-
                    sum
                    + int (
                        lattice.[idx i j size]
                        * (spinSum (i, j) lattice size)
                    )

        -sum / 2

    let totalMagnetization lattice =
        let mutable sum = 0

        for spin in lattice do
            sum <- sum + int spin

        sum

    let simulate (parameters: Parameters) (lattice: sbyte array) =
        let probabilities =
            [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |]

        let mutable energy =
            totalEnergy lattice parameters.LatticeSize
            |> float

        let mutable magnetization =
            totalMagnetization lattice |> float

        let steps =
            [|
                for _ in 1 .. parameters.Sweeps do
                    yield energy, magnetization

                    let i, j =
                        parameters.Rng
                        |> randomIndex parameters.LatticeSize

                    let spin =
                        lattice.[idx i j parameters.LatticeSize]

                    let dE =
                        2y
                        * spin
                        * (spinSum (i, j) lattice parameters.LatticeSize)

                    let dM = -2y * spin

                    if
                        dE < 0y
                        || (parameters.Rng.NextDouble() < probabilities.[int dE
                                                                         / 4
                                                                         + 2])
                    then
                        lattice.[idx i j parameters.LatticeSize] <- -spin
                        energy <- energy + float dE
                        magnetization <- magnetization + float dM
            |]

        let N =
            parameters.LatticeSize
            * parameters.LatticeSize

        let avgE =
            (steps |> Array.averageBy fst) / float N

        let avgM =
            (steps |> Array.averageBy (snd >> abs))
            / float N

        {
            Beta = parameters.Beta
            AvgE = avgE
            C = 0.0
            AvgM = avgM
            X = 0.0
        }
