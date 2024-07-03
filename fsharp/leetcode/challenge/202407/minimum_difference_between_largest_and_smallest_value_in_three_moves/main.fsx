let minDifference (nums: int list) : int =
    if List.length nums <= 4 then
        0
    else
        let nums' = List.sort nums

        List.zip (List.take 4 nums') (nums' |> List.rev |> List.take 4 |> List.rev)
        |> List.map (fun (a, b) -> b - a)
        |> List.min

// 0
minDifference [ 5; 3; 2; 4 ]

// 1
minDifference [ 1; 5; 0; 10; 14 ]

// 0
minDifference [ 3; 100; 20 ]
