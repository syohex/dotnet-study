let minimumLength (s: string) : int =
    s
    |> Seq.fold
        (fun (acc: int[]) c ->
            let index = int c - int 'a'
            acc.[index] <- acc.[index] + 1
            acc)
        (Array.zeroCreate 26)
    |> Array.fold
        (fun acc n ->
            if n = 0 then acc
            elif n % 2 = 1 then acc + 1
            else acc + 2)
        0

// 5
minimumLength "abaacbcbb"

// 2
minimumLength "aa"
