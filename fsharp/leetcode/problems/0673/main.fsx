let findNumberOfLIS (nums: int list) : int =
    let nums' = List.toArray nums
    let len = Array.length nums'

    let dpLen = Array.init len (fun _ -> 1)
    let dpCount = Array.init len (fun _ -> 1)

    for i in 0 .. (len - 1) do
        for j in 0 .. (i - 1) do
            if nums'.[j] < nums'.[i] then
                if dpLen.[j] + 1 > dpLen.[i] then
                    dpLen.[i] <- dpLen.[j] + 1
                    dpCount.[i] <- 0

                if dpLen.[j] + 1 = dpLen.[i] then
                    dpCount.[i] <- dpCount.[i] + dpCount.[j]

    let maxLen = Array.max dpLen

    dpLen
    |> Array.indexed
    |> Array.fold (fun acc (i, len) -> if len = maxLen then acc + dpCount.[i] else acc) 0

// 2
findNumberOfLIS [ 1; 3; 5; 4; 7 ]

// 5
findNumberOfLIS [ 2; 2; 2; 2; 2 ]
