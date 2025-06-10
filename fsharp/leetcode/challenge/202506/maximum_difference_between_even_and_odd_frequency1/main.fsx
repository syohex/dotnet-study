let maxDifference (s: string) : int =
    let freq =
        s
        |> Seq.fold
            (fun (acc: int[]) c ->
                let idx = int c - int 'a'
                acc.[idx] <- acc.[idx] + 1
                acc)
            (Array.zeroCreate 26)

    let maxOdd = freq |> Array.filter (fun n -> n % 2 = 1) |> Array.max
    let minEven = freq |> Array.filter (fun n -> n <> 0 && n % 2 = 0) |> Array.min
    maxOdd - minEven

// 3
maxDifference "aaaaabbc"

// 1
maxDifference "abcabcab"
