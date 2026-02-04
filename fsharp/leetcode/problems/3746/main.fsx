let minLengthAfterRemovals (s: string) : int =
    s
    |> Seq.fold (fun (a, b) c -> if c = 'a' then a + 1, b else a, b + 1) (0, 0)
    |> fun (a, b) -> abs (a - b)

// 0
minLengthAfterRemovals "aabbab"

// 4
minLengthAfterRemovals "aaaa"

// 1
minLengthAfterRemovals "aabbb"
