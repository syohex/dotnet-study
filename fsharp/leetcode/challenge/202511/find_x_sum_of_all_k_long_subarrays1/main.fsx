let findXSum (nums: int list) (k: int) (x: int) : int list =
    let topXFreq (x: int) (nums: int list) : int =
        nums
        |> List.countBy id
        |> List.sortBy snd
        |> List.rev
        |> List.take x
        |> List.fold (fun acc (n, freq) -> acc + n * freq) 0

    nums |> List.windowed k |> List.map (topXFreq x)

// [6, 10, 12]
findXSum [ 1; 1; 2; 2; 3; 4; 2; 3 ] 6 2

// [11;15;15;15;12]
findXSum [ 3; 8; 7; 8; 7; 5 ] 2 2
