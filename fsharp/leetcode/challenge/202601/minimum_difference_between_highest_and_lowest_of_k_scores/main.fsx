let minimumDifference (nums: int list) (k: int) : int =
    nums
    |> List.sort
    |> List.windowed k
    |> List.map (fun v -> List.last v - List.head v)
    |> List.min

// 0
minimumDifference [ 90 ] 1

// 2
minimumDifference [ 9; 4; 1; 7 ] 2
