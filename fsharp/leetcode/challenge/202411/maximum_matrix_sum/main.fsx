let maxMatrixSum (matrix: int[,]) : int64 =
    matrix
    |> Seq.cast<int>
    |> Seq.map int64
    |> Seq.fold
        (fun (sum, negatives, minVal) n -> sum + abs n, (negatives + if n < 0 then 1 else 0), min minVal (abs n))
        (0L, 0, System.Int64.MaxValue)
    |> fun (sum, negatives, minVal) -> if negatives % 2 = 0 then sum else sum - 2L * minVal

// 4
maxMatrixSum (array2D [ [ 1; -1 ]; [ -1; 1 ] ])

// 16
maxMatrixSum (array2D [ [ 1; 2; 3 ]; [ -1; -2; -3 ]; [ 1; 2; 3 ] ])

// 15
maxMatrixSum (array2D [ [ -1; 0; -1 ]; [ -2; 1; 3 ]; [ 3; 2; 2 ] ])
