let minimumRecolors (blocks: string) (k: int) : int =
    let countWhite = Array.filter ((=) 'W') >> Array.length

    blocks
    |> Seq.windowed k
    |> Seq.fold (fun acc v -> min acc (countWhite v)) blocks.Length

// 3
minimumRecolors "WBBWWBBWBW" 7

// 0
minimumRecolors "WBWBBBW" 2
