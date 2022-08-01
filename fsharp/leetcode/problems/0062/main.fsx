let uniquePaths (m: int) (n: int) : int =
    let dp = Array2D.zeroCreate m n
    dp.[0, 0] <- 1

    for i in 0 .. (m - 1) do
        for j in 0 .. (n - 1) do
            if i <> 0 then
                dp.[i, j] <- dp.[i, j] + dp.[i - 1, j]

            if j <> 0 then
                dp.[i, j] <- dp.[i, j] + dp.[i, j - 1]

    dp.[m - 1, n - 1]

// 28
uniquePaths 3 7

// 3
uniquePaths 3 2

// 1
uniquePaths 1 100
