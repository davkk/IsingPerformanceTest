namespace Version3

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b

open Helpers

type Ising(parameters: Parameters) =

    let mutable lattice =
        (fun _ -> if parameters.Rng.NextDouble() < 0.5 then 1y else -1y)
        |> Array.init (
            parameters.LatticeSize
            * parameters.LatticeSize
        )

    let idx (i: int) (j: int) = i + j * parameters.LatticeSize

    let randomIndex max =
        parameters.Rng.Next(0, max), parameters.Rng.Next(0, max)

    let spinSum (i, j) =
        lattice.[idx ((i - 1) %/ parameters.LatticeSize) j]
        + lattice.[idx ((i + 1) %/ parameters.LatticeSize) j]
        + lattice.[idx i ((j - 1) %/ parameters.LatticeSize)]
        + lattice.[idx i ((j + 1) %/ parameters.LatticeSize)]

    let totalEnergy () =
        let mutable sum = 0

        for i in 0 .. parameters.LatticeSize - 1 do
            for j in 0 .. parameters.LatticeSize - 1 do
                sum <-
                    sum
                    + int (lattice.[idx i j] * (spinSum (i, j)))

        -sum / 2

    let totalMagnetization () =
        let mutable sum = 0

        for spin in lattice do
            sum <- sum + int spin

        sum

    member this.Simulate () =
        let probabilities =
            [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |]

        let mutable energy = totalEnergy () |> float

        let mutable magnetization =
            totalMagnetization () |> float

        let steps =
            [|
                for _ in 1 .. parameters.Sweeps do
                    yield energy, magnetization

                    let i, j =
                        randomIndex parameters.LatticeSize

                    let spin = lattice.[idx i j]

                    let dE = 2y * spin * (spinSum (i, j))

                    let dM = -2y * spin

                    if
                        dE < 0y
                        || (parameters.Rng.NextDouble() < probabilities.[int dE / 4 + 2])
                    then
                        lattice.[idx i j] <- -spin
                        energy <- energy + float dE
                        magnetization <- magnetization + float dM
            |]

        let N = parameters.LatticeSize * parameters.LatticeSize

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
