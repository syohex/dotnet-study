let isPowerOfFour (n: int) : bool =
    seq { 0 .. 15 }
    |> Seq.tryFind (fun m -> System.Math.Pow(4, m) = n)
    |> Option.isSome

// true
isPowerOfFour 4
// true
isPowerOfFour 64
// true
isPowerOfFour 1
// false
isPowerOfFour 3
// false
isPowerOfFour 5
