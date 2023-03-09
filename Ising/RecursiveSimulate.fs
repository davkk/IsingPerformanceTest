namespace RecursiveSimulate

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

        let rec loop (sweep, energy, magnetization) =
            if sweep = parameters.Sweeps then
                let N =
                    parameters.LatticeSize
                    * parameters.LatticeSize

                let avgE = energy / float N

                let avgM = magnetization / float N

                {
                    Beta = parameters.Beta
                    AvgE = avgE
                    C = 0.0
                    AvgM = avgM
                    X = 0.0
                }
            else
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

                let accept =
                    dE < 0y
                    || (parameters.Rng.NextDouble() < probabilities.[int dE / 4
                                                                     + 2])

                if accept then
                    lattice.[idx i j parameters.LatticeSize] <- -spin

                    let energy' = energy + float dE

                    let magnetization' =
                        magnetization + float dM

                    loop (sweep + 1, energy', magnetization')
                else
                    loop (sweep + 1, energy, magnetization)

        let energy =
            totalEnergy lattice parameters.LatticeSize
            |> float

        let magnetization =
            totalMagnetization lattice |> float

        loop (0, energy, magnetization)
