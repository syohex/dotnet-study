let coloredCells (n: int) : int64 =
    seq { 1..n }
    |> Seq.fold (fun (acc: int64) n -> acc + 2L * (2L * int64 n - 1L)) (-2L * int64 n + 1L)

// 1
coloredCells 1

// 5
coloredCells 2

// 13
coloredCells 3

// 25
coloredCells 4
