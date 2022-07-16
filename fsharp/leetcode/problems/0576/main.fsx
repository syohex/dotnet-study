let findPaths (m: int) (n: int) (maxMove: int) (startRow: int) (startColumn: int) : int =
    let rec findPaths' row col moves cache =
        if row < 0 || row >= m || col < 0 || col >= n then
            1L, cache
        elif moves <= 0 then
            0L, cache
        else
            match Map.tryFind (m, n, moves) cache with
            | Some (v) -> v, cache
            | None ->
                let ret1, cache1 = findPaths' (row - 1) col (moves - 1) cache
                let ret2, cache2 = findPaths' row (col - 1) (moves - 1) cache1
                let ret3, cache3 = findPaths' (row + 1) col (moves - 1) cache2
                let ret4, cache4 = findPaths' row (col + 1) (moves - 1) cache3

                let ret = (ret1 + ret2 + ret3 + ret4) % 1_000_000_007L
                ret, Map.add (row, col, moves) ret cache4

    findPaths' startRow startColumn maxMove Map.empty
    |> fst
    |> int

// 6
findPaths 2 2 2 0 0

// 12
findPaths 1 3 3 0 1
