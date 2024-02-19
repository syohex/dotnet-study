open System.Numerics

let isPowerOfTwo (n: int) : bool =
    n >= 1 && BitOperations.PopCount(uint n) = 1

// true
isPowerOfTwo 1

// true
isPowerOfTwo 16

// false
isPowerOfTwo 3

// false
isPowerOfTwo -2
