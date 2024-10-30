let minimumMountainRemovals (nums: int[]) : int =
    let len = Array.length nums
    let dpAsc = Array.init len (fun _ -> 1)
    let dpDesc = Array.init len (fun _ -> 1)

    for i in 0 .. (len - 1) do
        for j in seq { 0 .. (i - 1) } |> Seq.rev do
            if nums.[j] < nums.[i] then
                dpAsc.[i] <- max dpAsc.[i] (dpAsc.[j] + 1)

    for i in seq { 0 .. (len - 1) } |> Seq.rev do
        for j in (i + 1) .. (len - 1) do
            if nums.[i] > nums.[j] then
                dpDesc.[i] <- max dpDesc.[i] (dpDesc.[j] + 1)

    seq { 0 .. (len - 1) }
    |> Seq.fold
        (fun acc i ->
            if dpAsc.[i] > 1 && dpDesc.[i] > 1 then
                min acc (len + 1 - dpAsc.[i] - dpDesc.[i])
            else
                acc)
        (len + 1)

// 0
minimumMountainRemovals [| 1; 3; 1 |]

// 3
minimumMountainRemovals [| 2; 1; 1; 5; 6; 2; 3; 1 |]

// 2
minimumMountainRemovals [| 9; 8; 1; 7; 6; 5; 4; 3; 2; 1 |]
