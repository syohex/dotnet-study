let maxChunksToSorted (arr: int list) : int =
    let rec maxChunksToSorted' maxWindows right acc =
        match maxWindows with
        | [] -> acc
        | (i, h) :: t ->
            let right = max h right
            let acc = if right <= i then acc + 1 else acc
            maxChunksToSorted' t right acc

    let maxWindows = arr |> List.indexed |> List.map (fun (i, n) -> i, max i n)

    maxChunksToSorted' maxWindows 0 0

// 1
maxChunksToSorted [ 4; 3; 2; 1; 1 ]

// 4
maxChunksToSorted [ 1; 0; 2; 3; 4 ]

// 2
maxChunksToSorted [ 0; 4; 2; 3; 1 ]
