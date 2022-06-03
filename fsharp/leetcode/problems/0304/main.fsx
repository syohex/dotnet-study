type NumMatrix =
    { Acc: int [,] }

    static member init(matrix: int [,]) : NumMatrix =
        let rows = Array2D.length1 matrix
        let cols = Array2D.length2 matrix

        let acc = Array2D.zeroCreate (rows + 1) cols

        for i in 0 .. (rows - 1) do
            for j in 0 .. (cols - 1) do
                acc.[i + 1, j] <- acc.[i, j] + matrix.[i, j]

        { Acc = acc }

    member this.sumRegion (row1: int) (col1: int) (row2: int) (col2: int) : int =
        seq { col1 .. col2 }
        |> Seq.fold (fun acc n -> acc + this.Acc.[row2 + 1, n] - this.Acc.[row1, n]) 0

let matrix1 =
    array2D [ [ 3; 0; 1; 4; 2 ]
              [ 5; 6; 3; 2; 1 ]
              [ 1; 2; 0; 1; 5 ]
              [ 4; 1; 0; 1; 7 ]
              [ 1; 0; 3; 0; 5 ] ]

let nm1 = NumMatrix.init matrix1

// 8
nm1.sumRegion 2 1 4 3

// 11
nm1.sumRegion 1 1 2 2

// 12
nm1.sumRegion 1 2 2 4
