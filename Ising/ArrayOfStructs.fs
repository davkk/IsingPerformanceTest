namespace ArrayOfStructs

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b
    let inline idx1d (i: int, j: int, size: int) = i + j * size
    let inline idx2d (k: int, size: int) = k % size, k / size

open Helpers

[<Struct>]
type Node =
    {
        mutable Spin: sbyte
        Neighbors: int * int * int * int
    }

    member self.FlipSpin () = self.Spin <- -self.Spin

[<Struct>]
type Lattice(nodes: Node array, size: int) =
    new(parameters: SimParams) =
        let size = parameters.LatticeSize

        let lattice =
            Array.zeroCreate<Node> (size * size)

        for k in 0 .. lattice.Length - 1 do
            let i, j = idx2d (k, size)

            lattice.[k] <-
                {
                    Spin = if parameters.Rng.NextFloat() < 0.5f then 1y else -1y
                    Neighbors =
                        idx1d ((i - 1) %/ size, j, size),
                        idx1d ((i + 1) %/ size, j, size),
                        idx1d (i, (j - 1) %/ size, size),
                        idx1d (i, (j + 1) %/ size, size)
                }

        Lattice(lattice, size)

    member _.Spins = nodes
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

    member self.SumNeighbors (k: int) =
        let left, right, top, down =
            self.Spins.[k].Neighbors

        self.Spins.[left].Spin
        + self.Spins.[right].Spin
        + self.Spins.[top].Spin
        + self.Spins.[down].Spin

module Lattice =
    let inline totalEnergy (lattice: Lattice) =
        let mutable sum = 0

        for k in 0 .. lattice.Spins.Length - 1 do
            sum <-
                sum
                + int (
                    lattice.Spins.[k].Spin
                    * lattice.SumNeighbors(k)
                )

        -float sum / 2.

    let inline totalMagnetization (lattice: Lattice) =
        let mutable sum = 0

        for node in lattice.Spins do
            sum <- sum + int node.Spin

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
                let randomIndex =
                    parameters.Rng.Next(0, lattice.Spins.Length)

                let node = lattice.Spins.[randomIndex]

                let dE =
                    2y
                    * node.Spin
                    * lattice.SumNeighbors(randomIndex)

                let dM = -2y * node.Spin

                let shouldFlip =
                    parameters.Rng.NextFloat() < probabilities.[int dE / 4 + 2]

                if dE < 0y || shouldFlip then
                    node.FlipSpin()

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
