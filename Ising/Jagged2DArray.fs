namespace Jagged2DArray

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b

open Helpers

[<Struct>]
type Lattice(spins: sbyte array array, size: int) =
    new(size: int, rng: System.Random) =
        let lattice =
            Array.zeroCreate<sbyte> (size * size)
            |> Array.chunkBySize size

        lattice
        |> Array.iteri (fun i row ->
            row
            |> Array.iteri (fun j _ ->
                lattice.[i].[j] <- if rng.NextDouble() > 0.5 then 1y else -1y
            )
        )

        Lattice(lattice, size)

    member _.Spins = spins
    member _.Size = size
    member _.NElems = size * size

    member self.SumNeighbors (i: int, j: int) =
        self.Spins.[(i - 1) %/ self.Size].[j]
        + self.Spins.[(i + 1) %/ size].[j]
        + self.Spins.[i].[(j - 1) %/ size]
        + self.Spins.[i].[(j + 1) %/ size]

module Lattice =
    let inline totalEnergy (lattice: Lattice) =
        let mutable sum = 0

        for i in 0 .. lattice.Size - 1 do
            for j in 0 .. lattice.Size - 1 do
                sum <-
                    sum
                    + int (
                        lattice.Spins.[i].[j]
                        * lattice.SumNeighbors(i, j)
                    )

        -float sum / 2.

    let inline totalMagnetization (lattice: Lattice) =
        lattice.Spins
        |> Array.sumBy (fun row ->
            row
            |> Array.sumBy (fun spin -> int spin)
        )
        |> float

module Ising =
    let simulate (parameters: Parameters) (lattice: Lattice) =
        let probabilities =
            [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |]

        let rec loop (sweep, energy, magnetization) =
            if sweep = parameters.Sweeps then
                {
                    Beta = parameters.Beta
                    AvgE = energy / float lattice.NElems
                    C = 0.0
                    AvgM = magnetization / float lattice.NElems
                    X = 0.0
                }

            else
                let i, j =
                    parameters.Rng.Next(0, lattice.Size),
                    parameters.Rng.Next(0, lattice.Size)

                let currentSpin = lattice.Spins.[i].[j]

                let dE =
                    2y
                    * currentSpin
                    * lattice.SumNeighbors(i, j)

                let dM = -2y * currentSpin

                let shouldFlip =
                    parameters.Rng.NextDouble() < probabilities.[int dE / 4 + 2]

                if dE < 0y || shouldFlip then
                    lattice.Spins.[i].[j] <- -currentSpin

                    loop (
                        sweep + 1,
                        energy + float dE,
                        magnetization + float dM
                    )
                else
                    loop (sweep + 1, energy, magnetization)

        let energy = lattice |> Lattice.totalEnergy

        let magnetization =
            lattice |> Lattice.totalMagnetization

        loop (0, energy, magnetization)
