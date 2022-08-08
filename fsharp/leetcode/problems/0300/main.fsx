let lengthOfLis (nums: int list) : int =
    let numsA = nums |> Array.ofList
    let dp = Array.init numsA.Length (fun _ -> 1)
    let mutable ret = 1

    for i in 1 .. (numsA.Length - 1) do
        for j in 0 .. (i - 1) do
            if numsA.[j] < numsA.[i] then
                dp.[i] <- System.Math.Max(dp.[i], dp.[j] + 1)

        ret <- System.Math.Max(ret, dp.[i])

    ret

// 4
lengthOfLis [ 10
              9
              2
              5
              3
              7
              101
              18 ]

// 4
lengthOfLis [ 0; 1; 0; 3; 2; 3 ]

// 1
lengthOfLis [ 7; 7; 7; 7; 7; 7 ]

// 1
lengthOfLis [ 1 ]
