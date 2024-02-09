let largestDivisibleSubset (nums: int list) : int list =
    let nums = nums |> List.sort |> List.toArray
    let len = Array.length nums

    let dp = Array.init len (fun i -> [ nums.[i] ])

    for i in 0 .. (len - 1) do
        for j in 0 .. (i - 1) do
            if nums.[i] % nums.[j] = 0 && List.length dp.[j] + 1 > List.length dp.[i] then
                dp.[i] <- nums.[i] :: dp.[j]

    dp |> Array.sortBy _.Length |> Array.rev |> Array.head |> List.rev

// [1,3]
largestDivisibleSubset [ 1; 2; 3 ]

// [1,2,4,8]
largestDivisibleSubset [ 1; 2; 4; 8 ]
