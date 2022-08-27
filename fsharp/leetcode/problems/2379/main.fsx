let minimumRecolors(blocks: string) (k: int) : int =
    let rec minimumRecolors' i sum (blocks: char[]) acc =
        if i >= blocks.Length then
            acc
        else
            let sum' = if blocks.[i - k] = 'W' then sum - 1 else sum
            let sum'' = if blocks.[i] = 'W' then sum' + 1 else sum'
            minimumRecolors' (i + 1) sum'' blocks (System.Math.Min(acc, sum''))

    let cs = blocks |> Seq.toArray
    let sum = cs |> Array.take k |> Array.filter (fun c -> c = 'W') |> Array.length
    minimumRecolors' k sum cs sum

// 3
minimumRecolors "WBBWWBBWBW" 7

// 0
minimumRecolors "WBWBBBW" 2
