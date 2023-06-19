let largestAltitude (gain: int list) : int =
    gain
    |> List.fold
        (fun (alt, ret) n ->
            let alt' = alt + n
            alt', System.Math.Max(ret, alt))
        (0, 0)
    |> snd

// 1
largestAltitude [ -5; 1; 5; 0; -7 ]

// 0
largestAltitude [ -4; -3; -2; -1; 4; 3; 2 ]
