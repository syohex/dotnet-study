let bestTeamScore (scores: int list) (ages: int list) : int =
    let len = List.length scores
    let ageScores = List.zip ages scores |> List.sort |> List.toArray

    let dp =
        ageScores
        |> Array.indexed
        |> Array.fold
            (fun (acc: int[]) (i, (_, score)) ->
                acc.[i] <- score
                acc)
            (Array.zeroCreate len)

    let mutable ret = 0

    for i in 0 .. (len - 1) do
        for j in 0 .. (i - 1) do
            if (snd ageScores.[i]) >= (snd ageScores.[j]) then
                dp.[i] <- System.Math.Max(dp.[i], dp.[j] + (snd ageScores.[i]))

        ret <- System.Math.Max(ret, dp.[i])

    ret

// 34
bestTeamScore [ 1; 3; 5; 10; 15 ] [ 1; 2; 3; 4; 5 ]

// 16
bestTeamScore [ 4; 5; 6; 5 ] [ 2; 1; 2; 1 ]

// 6
bestTeamScore [ 1; 2; 3; 5 ] [ 8; 9; 10; 1 ]

// 4
bestTeamScore [ 4 ] [ 1 ]
