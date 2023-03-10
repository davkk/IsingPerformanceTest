namespace Jagged2DArray

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b

open Helpers

[<Struct>]
type Lattice(spins: sbyte array array, size: int) =
    new(parameters: SimParams) =
        let size = parameters.LatticeSize

        let lattice =
            Array.zeroCreate<sbyte> size
            |> Array.create size

        for i in 0 .. size - 1 do
            for j in 0 .. size - 1 do
                lattice.[i].[j] <-
                    if parameters.Rng.NextFloat() < 0.5f then 1y else -1y

        Lattice(lattice, size)

    member _.Spins = spins
    member _.Size = size

    member inline r.Item
        with get (i: int) = r.Spins[i]

        and set (index: int) value = r.Spins[index] <- value

module Lattice =
    let inline sumNeighbors (i: int, j: int) (lattice: Lattice) =
        lattice.[(i - 1) %/ lattice.Size].[j]
        + lattice.[(i + 1) %/ lattice.Size].[j]
        + lattice.[i].[(j - 1) %/ lattice.Size]
        + lattice.[i].[(j + 1) %/ lattice.Size]

    let inline totalEnergy (lattice: Lattice) =
        let mutable sum = 0

        for i in 0 .. lattice.Size - 1 do
            for j in 0 .. lattice.Size - 1 do
                let neighborSum =
                    lattice |> sumNeighbors (i, j)

                sum <-
                    sum
                    + int (lattice.[i].[j] * neighborSum)

        -float sum / 2.0

    let inline totalMagnetization (lattice: Lattice) =
        let mutable sum = 0

        for i in 0 .. lattice.Size - 1 do
            for j in 0 .. lattice.Size - 1 do
                sum <- sum + int lattice.[i].[j]

        float sum

module Ising =
    let simulate (parameters: SimParams) (lattice: Lattice) =
        let probabilities =
            [|
                for dE in -8. .. 4. .. 8. ->
                    exp (-parameters.Beta * dE) |> float32
            |]

        let rec loop (sweep, energy, magnetization) =
            if sweep = parameters.Sweeps then
                {
                    Beta = parameters.Beta
                    AvgE = energy / float lattice.Spins.Length
                    C = 0.0
                    AvgM =
                        magnetization
                        / float lattice.Spins.Length
                    X = 0.0
                }

            else
                let i, j =
                    parameters.Rng.Next(0, lattice.Size),
                    parameters.Rng.Next(0, lattice.Size)

                let mutable spin = lattice.Spins.[i].[j]

                let neighborSum =
                    lattice |> Lattice.sumNeighbors (i, j)

                let dE = 2y * spin * neighborSum

                let dM = -2y * spin

                let shouldFlip =
                    parameters.Rng.NextFloat() < probabilities.[int dE / 4 + 2]

                if dE < 0y || shouldFlip then
                    spin <- -spin

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
