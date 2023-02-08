open System

let jump (nums: int list) : int =
    let nums' = Array.ofList nums
    let len = nums'.Length
    let dp = Array.init len (fun _ -> len + 1)
    dp.[0] <- 0

    for i in 0 .. (len - 1) do
        for j in 1 .. nums'.[i] do
            if i + j < len then
                dp.[i + j] <- Math.Min(dp.[i + j], dp.[i] + 1)

    Array.last dp

// 2
jump [ 2; 3; 1; 1; 4 ]

// 2
jump [ 2; 3; 0; 1; 4 ]

// 1
jump [ 9; 9; 9; 9; 9 ]
