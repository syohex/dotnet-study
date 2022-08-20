let minRefuelStops (target: int) (startFuel: int) (stations: (int * int) []) : int =
    let len = stations.Length
    let dp = Array.zeroCreate (len + 1)
    dp.[0] <- startFuel

    for i in 0 .. (len - 1) do
        for j in seq { 0..i } |> Seq.rev do
            if dp.[j] >= fst stations.[i] then
                dp.[j + 1] <- System.Math.Max(dp.[j + 1], dp.[j] + (snd stations.[i]))

    match dp
          |> Array.mapi (fun i v -> i, v)
          |> Array.tryFind (fun (_, v) -> v >= target)
        with
    | None -> -1
    | Some ((i, _)) -> i

// 0
minRefuelStops 1 1 [||]

// -1
minRefuelStops 100 1 [| (10, 100) |]

// 2
minRefuelStops
    100
    10
    [| (10, 60)
       (20, 30)
       (30, 30)
       (60, 40) |]


// 1
minRefuelStops 100 50 [| (25, 25); (50, 50) |]
