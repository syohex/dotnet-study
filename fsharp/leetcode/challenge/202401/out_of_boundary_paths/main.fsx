let findPaths (m: int) (n: int) (maxMove: int) (startRow: int) (startColumn: int) : int =
    let rec findPaths' row col moves cache =
        if row < 0 || row >= m || col < 0 || col >= n then
            1, cache
        elif moves = 0 then
            0, cache
        else
            match Map.tryFind (row, col, moves) cache with
            | Some(v) -> v, cache
            | None ->
                let moves' = moves - 1
                let a1, cache = findPaths' (row - 1) col moves' cache
                let a2, cache = findPaths' row (col - 1) moves' cache
                let a3, cache = findPaths' (row + 1) col moves' cache
                let a4, cache = findPaths' row (col + 1) moves' cache
                let sum = ([ a1; a2; a3; a4 ] |> List.map int64 |> List.sum)
                let ret = sum % 1_000_000_007L |> int
                ret, Map.add (row, col, moves) ret cache

    findPaths' startRow startColumn maxMove Map.empty |> fst

// 6
findPaths 2 2 2 0 0

// 12
findPaths 1 3 3 0 1
