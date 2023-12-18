let maxProductDifference (nums: int list) : int =
    let sorted = List.sort nums
    let max = sorted |> List.rev |> List.take 2 |> List.reduce (*)
    let min = sorted |> List.take 2 |> List.reduce (*)
    max - min

// 34
maxProductDifference [ 5; 6; 2; 7; 4 ]

// 64
maxProductDifference [ 4; 2; 5; 9; 7; 4; 8 ]
