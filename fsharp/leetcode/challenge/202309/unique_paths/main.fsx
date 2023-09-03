let uniquePath (m: int) (n: int) : int =
    let dp = Array2D.zeroCreate m n
    dp.[0, 0] <- 1

    dp
    |> Array2D.iteri (fun i j v ->
        let v' = if i >= 1 then v + dp.[i - 1, j] else v
        let v'' = if j >= 1 then v' + dp.[i, j - 1] else v'
        dp.[i, j] <- v'')

    dp.[m - 1, n - 1]

// 28
uniquePath 3 7

// 3
uniquePath 3 2
