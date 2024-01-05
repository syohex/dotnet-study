let lengthOfLIS (nums: int list) : int =
    let nums' = Array.ofList nums
    let len = Array.length nums'
    let dp = Array.init len (fun _ -> 1)

    for i in 1 .. (len - 1) do
        for j in 0 .. (i - 1) do
            if nums'.[i] > nums'.[j] && dp.[j] + 1 > dp.[i] then
                dp.[i] <- dp.[j] + 1

    Array.max dp

// 4
lengthOfLIS [ 10; 9; 2; 5; 3; 7; 101; 18 ]

// 4
lengthOfLIS [ 0; 1; 0; 3; 2; 3 ]

// 1
lengthOfLIS [ 7; 7; 7; 7; 7; 7; 7 ]

// 6
lengthOfLIS [ 3; 5; 6; 2; 5; 4; 19; 5; 6; 7; 12 ]
