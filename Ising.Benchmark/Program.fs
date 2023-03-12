open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Diagnosers

open Domain
open Xoshiro.PRNG64

[<MemoryDiagnoser>]
[<MedianColumn>]
[<HardwareCounters(HardwareCounter.CacheMisses,
                   HardwareCounter.BranchInstructions,
                   HardwareCounter.BranchMispredictions)>]
// [<EventPipeProfiler(EventPipeProfile.CpuSampling)>]
type Benchmarks() =

    let seed = 2001
    let sweeps = 10_000_000
    let latticeSize = 256
    let beta = 1.4

    let parameters: Parameters =
        {
            Rng = System.Random(seed)
            Sweeps = sweeps
            LatticeSize = latticeSize
            NumOfStates = 2
            Beta = beta
        }

    let simParams: SimParams =
        {
            Rng = XoRoShiRo128plus(seed)
            Sweeps = sweeps
            LatticeSize = latticeSize
            Beta = beta
        }

    let simParams2: SimParams2 =
        {
            Rng = XoRoShiRo128plus(seed)
            Sweeps = sweeps
            LatticeSize = uint16 latticeSize
            Beta = beta
        }


    // [<Benchmark>]
    member _.Initial () =
        let lattice =
            Initial.Ising.initLattice parameters

        lattice
        |> Initial.Ising.simulate parameters

    // [<Benchmark>]
    member _.TensorToArray () =
        let lattice =
            TensorToArray.Ising.initLattice
                parameters.LatticeSize
                parameters.Rng

        lattice
        |> TensorToArray.Ising.simulate parameters

    // [<Benchmark>]
    member _.SbyteArray () =
        let lattice =
            SbyteArray.Ising.initLattice parameters.LatticeSize parameters.Rng

        lattice
        |> SbyteArray.Ising.simulate parameters

    // [<Benchmark>]
    member _.ClassIsing () =
        let ising = ClassIsing.Ising(parameters)
        ising.Simulate()

    // [<Benchmark>]
    member _.RecursiveSimulate () =
        let lattice =
            RecursiveSimulate.Ising.initLattice
                parameters.LatticeSize
                parameters.Rng

        lattice
        |> RecursiveSimulate.Ising.simulate parameters

    // [<Benchmark>]
    member _.LatticeWrapper () =
        LatticeWrapper.Lattice(parameters.LatticeSize, parameters.Rng)
        |> LatticeWrapper.Ising.simulate parameters

    [<Benchmark>]
    member _.XorshiftRandom () =
        XorshiftRandom.Lattice(simParams)
        |> XorshiftRandom.Ising.simulate simParams

    // [<Benchmark>]
    member _.ArrayOfStructs () =
        ArrayOfStructs.Lattice(simParams)
        |> ArrayOfStructs.Ising.simulate simParams

    // [<Benchmark>]
    member _.NeighborsArray () =
        NeighborsArray.Lattice(simParams)
        |> NeighborsArray.Ising.simulate simParams

    // [<Benchmark>]
    member _.StructTuples () =
        StructTuples.Lattice(simParams)
        |> StructTuples.Ising.simulate simParams

    // [<Benchmark>]
    member _.Final () =
        Final.Lattice(simParams)
        |> Final.Ising.simulate simParams

    [<Benchmark>]
    member _.BetterXorshift () =
        BetterXorshift.Lattice(simParams2)
        |> BetterXorshift.Ising.simulate simParams2

    [<Benchmark>]
    member _.Jagged2DArray () =
        Jagged2DArray.Lattice(simParams)
        |> Jagged2DArray.Ising.simulate simParams

[<EntryPoint>]
let main _ =

    let _ = BenchmarkRunner.Run<Benchmarks>()

    0
