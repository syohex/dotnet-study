let countVowelPermutation (n: int) : int =
    let dp = Array2D.init 5 n (fun _ _ -> 0L)

    for i in 0..4 do
        dp.[i, 0] <- 1L

    let MOD = 1_000_000_007L

    for i in 1 .. (n - 1) do
        dp.[0, i] <- dp.[1, i - 1]
        dp.[1, i] <- (dp.[0, i - 1] + dp.[2, i - 1]) % MOD

        dp.[2, i] <-
            (dp.[0, i - 1]
             + dp.[1, i - 1]
             + dp.[3, i - 1]
             + dp[4, i - 1]) % MOD

        dp.[3, i] <- (dp.[2, i - 1] + dp.[4, i - 1]) % MOD
        dp.[4, i] <- dp.[0, i - 1]

    seq { 0..4 }
    |> Seq.fold (fun acc i -> (acc + dp.[i, n - 1]) % MOD) 0L
    |> int

// 5
countVowelPermutation 1

// 10
countVowelPermutation 2

// 68
countVowelPermutation 5
