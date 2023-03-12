namespace BetterXorshift

open System.Runtime.InteropServices
open System.Buffers
open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b
    let inline idx (i: uint16, j: uint16, size: uint16) = int (i + j * size)

open Helpers

[<Struct; StructLayout(LayoutKind.Sequential, Pack = 1)>]
type Spin =
    {
        mutable Index2D: struct (uint16 * uint16)
        mutable Value: sbyte
    }

[<Struct>]
type Lattice(spins: Spin array, size: uint16) =
    new(parameters: SimParams2) =
        let size = parameters.LatticeSize

        let pool = ArrayPool<Spin>.Shared
        let lattice = pool.Rent(int size * int size);
        pool.Return(lattice)

        for k in 0 .. lattice.Length - 1 do
            lattice.[k].Value <-
                if parameters.Rng.NextFloat() < 0.5f then 1y else -1y

            lattice.[k].Index2D <- uint16 k % size, uint16 k / size

        Lattice(lattice, size)

    member _.Spins = spins
    member _.Size = size

module Lattice =
    let inline sumNeighbors (i: uint16, j: uint16) (lattice: Lattice) =
        lattice.Spins.[idx ((i - 1us) %/ lattice.Size, j, lattice.Size)].Value
        + lattice.Spins.[idx ((i + 1us) %/ lattice.Size, j, lattice.Size)].Value
        + lattice.Spins.[idx (i, (j - 1us) %/ lattice.Size, lattice.Size)].Value
        + lattice.Spins.[idx (i, (j + 1us) %/ lattice.Size, lattice.Size)].Value

    let inline totalEnergy (lattice: Lattice) =
        let mutable sum = 0

        for k in 0 .. lattice.Spins.Length - 1 do
            let spin = lattice.Spins.[k]
            let struct (i, j) = spin.Index2D

            let neighborSum =
                lattice |> sumNeighbors (i, j)

            sum <- sum + int (spin.Value * neighborSum)

        -float sum / 2.0

    let inline totalMagnetization (lattice: Lattice) =
        let mutable sum = 0

        for spin in lattice.Spins do
            sum <- sum + int spin.Value

        float sum

module Ising =
    let simulate (parameters: SimParams2) (lattice: Lattice) =
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
                let randomIndex =
                    parameters.Rng.Next(0, lattice.Spins.Length)

                let mutable spin =
                    lattice.Spins.[randomIndex]

                let struct (i, j) = spin.Index2D
                let neighborSum = lattice |> Lattice.sumNeighbors (i,j)

                let dE =
                    2y
                    * spin.Value
                    * neighborSum

                let dM = -2y * spin.Value

                let shouldFlip =
                    parameters.Rng.NextFloat() < probabilities.[int dE / 4 + 2]

                if dE < 0y || shouldFlip then
                    spin.Value <- -spin.Value

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
