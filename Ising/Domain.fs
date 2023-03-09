module Domain

type Parameters =
    {
        Rng: System.Random
        Sweeps: int
        LatticeSize: int
        NumOfStates: int
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
