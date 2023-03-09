namespace Initial

open Tensor
open FSharp.Stats

open Domain

module private Helpers =
    let inline (%/) a b = (a + b) % b

    let inline randomIndex max (rng: System.Random) =
        rng.NextInt64(0, max), rng.NextInt64(0, max)

    let inline spinSum (i, j) (lattice: Tensor<int>) =
        let L = lattice.Shape[0]

        lattice.[[ (i - 1L) %/ L; j ]]
        + lattice.[[ (i + 1L) %/ L; j ]]
        + lattice.[[ i; (j - 1L) %/ L ]]
        + lattice.[[ i; (j + 1L) %/ L ]]

open Helpers

module Ising =

    let initLattice parameters =
        HostTensor.init
            [ parameters.LatticeSize; parameters.LatticeSize ]
            (fun _ -> if parameters.Rng.NextDouble() < 0.5 then -1 else 1)

    let totalEnergy (lattice: Tensor<int>) =
        -(lattice
          |> HostTensor.mapi (fun index spin ->
              spin * lattice
              |> spinSum (index[0], index[1])
          )
          |> Tensor.sum)
        / 2

    let simulate (parameters: Parameters) lattice =
        let probabilities =
            [| for dE in -8. .. 4. .. 8. -> exp (-parameters.Beta * dE) |]

        let mutable energy =
            totalEnergy lattice |> float

        let mutable magnetization =
            lattice |> Tensor.sum |> float

        let steps =
            [|
                for _ in 1 .. parameters.Sweeps do
                    yield energy, magnetization

                    let i, j =
                        parameters.Rng
                        |> randomIndex parameters.LatticeSize

                    let spin = lattice.[[ i; j ]]

                    let dE =
                        2 * spin * (lattice |> spinSum (i, j))

                    let dM = -2 * spin

                    if
                        dE < 0
                        || (parameters.Rng.NextDouble() < probabilities.[dE / 4
                                                                         + 2])
                    then
                        lattice.[[ i; j ]] <- -spin
                        energy <- energy + float dE
                        magnetization <- magnetization + float dM
            |]

        let avgE =
            (steps |> Array.averageBy fst)
            / float lattice.NElems

        let C =
            (steps |> Array.map fst |> Seq.stDev)
            * parameters.Beta ** 2.
            / float lattice.NElems

        let avgM =
            (steps |> Array.averageBy (snd >> abs))
            / float lattice.NElems

        let X =
            (steps
             |> Array.map (snd >> abs)
             |> Seq.stDev)
            * parameters.Beta
            / float lattice.NElems

        {
            Beta = parameters.Beta
            AvgE = avgE
            C = C
            AvgM = avgM
            X = X
        }
