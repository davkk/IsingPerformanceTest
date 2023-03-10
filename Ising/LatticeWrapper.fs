namespace LatticeWrapper

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b
    let inline idx2 (k: int) (size: int) = k % size, k / size

open Helpers

[<Struct>]
type Lattice(spins: sbyte array, size: int) =
    new(size: int, rng: System.Random) =
        let lattice =
            Array.zeroCreate<sbyte> (size * size)

        lattice
        |> Array.iteri (fun i _ ->
            lattice.[i] <- if rng.NextDouble() > 0.5 then 1y else -1y
        )

        Lattice(lattice, size)

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

    member self.SumNeighbors (k: int) =
        let i, j = Helpers.idx2 k self.Size
        self.SumNeighbors(i, j)

module Lattice =
    let inline totalEnergy (lattice: Lattice) =
        let mutable sum = 0

        for i in 0 .. lattice.Spins.Length - 1 do
            sum <-
                sum
                + int (
                    lattice.Spins.[i]
                    * lattice.SumNeighbors i
                )

        -float sum / 2.

    let inline totalMagnetization (lattice: Lattice) =
        let mutable sum = 0

        for spin in lattice.Spins do
            sum <- sum + int spin

        float sum

module Ising =
    let simulate (parameters: Parameters) (lattice: Lattice) =
        let probabilities =
            [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |]

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

                let currentSpin =
                    lattice.Spins.[randomIndex]

                let dE =
                    2y
                    * currentSpin
                    * lattice.SumNeighbors randomIndex

                let dM = -2y * currentSpin

                let shouldFlip =
                    parameters.Rng.NextDouble() < probabilities.[int dE / 4 + 2]

                if dE < 0y || shouldFlip then
                    lattice.Spins.[randomIndex] <- -currentSpin

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
