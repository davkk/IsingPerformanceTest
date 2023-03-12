module Domain

open Xoshiro.PRNG64

[<Struct>]
type Parameters =
    {
        Rng: System.Random
        Sweeps: int
        LatticeSize: int
        NumOfStates: int
        Beta: float
    }

[<Struct>]
type SimParams =
    {
        Rng: XoRoShiRo128plus
        Sweeps: int
        LatticeSize: int
        Beta: float
    }

[<Struct>]
type SimParams2 =
    {
        Rng: XoRoShiRo128plus
        Sweeps: int
        LatticeSize: uint16
        Beta: float
    }

[<Struct>]
type Stats =
    {
        Beta: float
        AvgE: float
        C: float
        AvgM: float
        X: float
    }
