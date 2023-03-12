namespace Final

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b

open Helpers

[<Struct>]
type Lattice(spins: sbyte array, size: int) =
    new(parameters: SimParams) =
        let lattice =
            Array.zeroCreate<sbyte> (parameters.LatticeSize * parameters.LatticeSize)

        lattice
        |> Array.iteri (fun i _ ->
            lattice.[i] <- if parameters.Rng.NextFloat() < 0.5f then 1y else -1y
        )

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

    member self.SumNeighbors (i: int, j: int) =
        self.Spin((i - 1) %/ self.Size, j)
        + self.Spin((i + 1) %/ self.Size, j)
        + self.Spin(i, (j - 1) %/ self.Size)
        + self.Spin(i, (j + 1) %/ self.Size)

module Lattice =
    let inline totalEnergy (lattice: Lattice) =
        let mutable sum = 0

        for i in 0 .. lattice.Size - 1 do
            for j in 0 .. lattice.Size - 1 do
                sum <-
                    sum
                    + int (
                        lattice.Spin(i, j)
                        * lattice.SumNeighbors(i, j)
                    )

        -float sum / 2.

    let inline totalMagnetization (lattice: Lattice) =
        let mutable sum = 0

        for spin in lattice.Spins do
            sum <- sum + int spin

        float sum

module Ising =
    let simulate (parameters: SimParams) (lattice: Lattice) =
        let probabilities =
            [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |> float32 |]

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

                let mutable spin =
                    lattice.Spin(i, j)

                let dE =
                    2y
                    * spin
                    * lattice.SumNeighbors(i, j)

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
