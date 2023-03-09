open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Diagnosers

open Domain

[<MemoryDiagnoser>]
[<MedianColumn>]
[<HardwareCounters(HardwareCounter.CacheMisses,
                   HardwareCounter.BranchInstructions,
                   HardwareCounter.BranchMispredictions)>]
[<EventPipeProfiler(EventPipeProfile.CpuSampling)>]
type Benchmarks() =

    let parameters: Parameters =
        {
            Rng = System.Random(2001)
            Sweeps = 1_000_000
            LatticeSize = 128
            NumOfStates = 2
            Beta = 1.4
        }

    [<Benchmark>]
    member _.Initial () =
        let lattice =
            Initial.Ising.initLattice parameters

        lattice |> Initial.Ising.simulate parameters

    [<Benchmark>]
    member _.TensorToArray () =
        let lattice =
            TensorToArray.Ising.initLattice parameters.LatticeSize parameters.Rng

        lattice |> TensorToArray.Ising.simulate parameters

    [<Benchmark>]
    member _.SbyteArray () =
        let lattice =
            SbyteArray.Ising.initLattice parameters.LatticeSize parameters.Rng

        lattice |> SbyteArray.Ising.simulate parameters

    [<Benchmark>]
    member _.ClassIsing () =
        let ising = ClassIsing.Ising(parameters)
        ising.Simulate()

    [<Benchmark>]
    member _.RecursiveSimulate () =
        let lattice =
            RecursiveSimulate.Ising.initLattice parameters.LatticeSize parameters.Rng

        lattice |> RecursiveSimulate.Ising.simulate parameters

[<EntryPoint>]
let main _ =

    let _ = BenchmarkRunner.Run<Benchmarks>()

    0
