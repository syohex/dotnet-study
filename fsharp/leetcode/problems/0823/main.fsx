let numFactoredBinaryTrees (arr: int []) : int =
    let arr' = arr |> Array.sort

    let h =
        arr'
        |> Array.mapi (fun i n -> i, n)
        |> Array.fold (fun acc (i, n) -> Map.add n i acc) Map.empty

    let dp = Array.init arr'.Length (fun _ -> 1L)
    let MOD = 1_000_000_007L

    for i in 1 .. (arr'.Length - 1) do
        for j in 0 .. (i - 1) do
            if arr'.[i] % arr'.[j] = 0 then
                match Map.tryFind (arr'.[i] / arr'.[j]) h with
                | Some (v) -> dp.[i] <- (dp.[i] + (dp.[j] * dp.[v])) % MOD
                | None -> ()

    dp
    |> Array.fold (fun acc n -> (acc + n) % MOD) 0L
    |> int

// 3
numFactoredBinaryTrees [| 2; 4 |]

// 7
numFactoredBinaryTrees [| 2; 4; 5; 10 |]
