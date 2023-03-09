namespace TensorToArray

open FSharp.Stats

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b

    let inline randomIndex max (rng: System.Random) =
        rng.Next(0, max), rng.Next(0, max)

    let inline idx (i: int) (j: int) (size: int) = i + j * size

    let inline spinSum (i, j) (lattice: array<int>) size =
        lattice.[idx ((i - 1) %/ size) j size]
        + lattice.[idx ((i + 1) %/ size) j size]
        + lattice.[idx i ((j - 1) %/ size) size]
        + lattice.[idx i ((j + 1) %/ size) size]

open Helpers

module Ising =

    let initLattice (size: int64) (rng: System.Random) =
        (fun _ -> if rng.NextDouble() < 0.5 then 1 else -1)
        |> Array.init (int size * int size)

    let totalEnergy (lattice: array<int>) size =
        let mutable sum = 0

        for i in 0 .. size - 1 do
            for j in 0 .. size - 1 do
                sum <-
                    sum
                    + (lattice.[idx i j size]
                       * (spinSum (i, j) lattice size))

        -sum / 2


    let simulate (parameters: Parameters) lattice =
        let probabilities =
            [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |]

        let mutable energy =
            totalEnergy lattice parameters.LatticeSize
            |> float

        let mutable magnetization =
            lattice |> Array.sum |> float

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
                        2
                        * spin
                        * (spinSum (i, j) lattice parameters.LatticeSize)

                    let dM = -2 * spin

                    if
                        dE < 0
                        || (parameters.Rng.NextDouble() < probabilities.[dE / 4
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

        let C =
            (steps |> Array.map fst |> Seq.stDev)
            * parameters.Beta ** 2.
            / float N

        let avgM =
            (steps |> Array.averageBy (snd >> abs))
            / float N

        let X =
            (steps
             |> Array.map (snd >> abs)
             |> Seq.stDev)
            * parameters.Beta
            / float N

        {
            Beta = parameters.Beta
            AvgE = avgE
            C = C
            AvgM = avgM
            X = X
        }
