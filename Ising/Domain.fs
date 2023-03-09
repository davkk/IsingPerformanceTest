module Domain

open Xoshiro.PRNG64

type Parameters =
    {
        Rng: System.Random
        Sweeps: int
        LatticeSize: int
        NumOfStates: int
        Beta: float
    }

type SimParams =
    {
        Rng: XoRoShiRo128plus
        Sweeps: int
        LatticeSize: int
        Beta: float
    }

type Stats =
    {
        Beta: float
        AvgE: float
        C: float
        AvgM: float
        X: float
    }
