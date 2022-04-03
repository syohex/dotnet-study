let minBitFlips (start: int) (goal: int) : int =
    (start ^^^ goal)
    |> uint32
    |> System.Runtime.Intrinsics.X86.Popcnt.PopCount
    |> int

// 3
minBitFlips 10 7

// 3
minBitFlips 3 4
