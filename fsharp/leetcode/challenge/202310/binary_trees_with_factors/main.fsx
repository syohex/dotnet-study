let numFactoredBinaryTrees (arr: int[]) : int =
    let modulo = 1_000_000_007L

    let arr = Array.sort arr

    let valueIndex =
        arr |> Array.indexed |> Array.fold (fun acc (i, n) -> Map.add n i acc) Map.empty

    let len = arr.Length
    let dp = Array.init len (fun _ -> 1L)

    for i in 0 .. (len - 1) do
        for j in 0 .. (i - 1) do
            if arr.[i] % arr.[j] = 0 then
                match Map.tryFind (arr.[i] / arr.[j]) valueIndex with
                | Some(other) -> dp.[i] <- (dp.[i] + (dp.[j] * dp.[other])) % modulo
                | None -> ()

    dp |> Array.fold (fun acc n -> (acc + n) % modulo) 0L |> int

// 3
numFactoredBinaryTrees [| 2; 4 |]

// 7
numFactoredBinaryTrees [| 2; 4; 5; 10 |]
