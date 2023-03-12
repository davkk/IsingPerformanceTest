namespace StructTuples

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b
    let inline idx1d (i: int, j: int, size: int) = i + j * size
    let inline idx2d (k: int, size: int) = k % size, k / size

open Helpers

[<Struct>]
type Lattice(spins: sbyte array, size: int) =
    new(parameters: SimParams) =
        let lattice =
            Array.zeroCreate<sbyte> (
                parameters.LatticeSize
                * parameters.LatticeSize
            )

        for k in 0 .. lattice.Length - 1 do
            lattice.[k] <- if parameters.Rng.NextFloat() < 0.5f then 1y else -1y

        Lattice(lattice, parameters.LatticeSize)

    member _.Spins = spins
    member _.Size = size

    member self.Spin (i: int, j: int) =
        if
            i < 0
            || i >= self.Size
            || j < 0
            || j >= self.Size
        then
            raise
            <| System.IndexOutOfRangeException
                $"Either i={i} or j={j} was out of range"

        self.Spins.[i + j * self.Size]

module Lattice =
    let inline totalMagnetization (lattice: Lattice) =
        let mutable sum = 0

        for spin in lattice.Spins do
            sum <- sum + int spin

        float sum

module Ising =
    let simulate (parameters: SimParams) (lattice: Lattice) =
        let probabilities =
            [|
                for dE in -8. .. 4. .. 8. ->
                    exp (-parameters.Beta * dE) |> float32
            |]

        let neighbors =
            Array.zeroCreate<struct (int * int * int * int)>
                lattice.Spins.Length

        for k in 0 .. lattice.Spins.Length - 1 do
            let i, j = idx2d (k, lattice.Size)

            neighbors.[k] <-
                struct (idx1d ((i - 1) %/ lattice.Size, j, lattice.Size),
                        idx1d ((i + 1) %/ lattice.Size, j, lattice.Size),
                        idx1d (i, (j - 1) %/ lattice.Size, lattice.Size),
                        idx1d (i, (j + 1) %/ lattice.Size, lattice.Size))

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
                let randomIndex =
                    parameters.Rng.Next(0, lattice.Spins.Length)

                let mutable spin =
                    lattice.Spins.[randomIndex]

                let neighborsSum =
                    let struct (left, right, top, down) =
                        neighbors.[randomIndex]

                    lattice.Spins.[left]
                    + lattice.Spins.[right]
                    + lattice.Spins.[top]
                    + lattice.Spins.[down]

                let dE = 2y * spin * neighborsSum

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

        let energy =
            let mutable sum = 0

            for k in 0 .. lattice.Spins.Length - 1 do
                let struct (left, right, top, down) =
                    neighbors.[k]

                let neighborsSum =
                    lattice.Spins.[left]
                    + lattice.Spins.[right]
                    + lattice.Spins.[top]
                    + lattice.Spins.[down]

                sum <-
                    sum
                    + int (lattice.Spins.[k] * neighborsSum)

            -float sum / 2.

        let magnetization =
            lattice |> Lattice.totalMagnetization

        loop (0, energy, magnetization)
