let sortMatrix (grid: int[,]) : int[,] =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    let rec getDiagonal row col acc =
        if row >= rows || col >= cols then
            List.rev acc
        else
            getDiagonal (row + 1) (col + 1) (grid.[row, col] :: acc)

    seq { 0 .. (rows - 1) }
    |> Seq.iter (fun i ->
        let v = getDiagonal i 0 []

        v
        |> List.sort
        |> List.rev
        |> List.indexed
        |> List.iter (fun (j, n) -> grid.[i + j, j] <- n))

    seq { 1 .. (cols - 1) }
    |> Seq.iter (fun i ->
        let v = getDiagonal 0 i []

        v |> List.sort |> List.indexed |> List.iter (fun (j, n) -> grid.[j, i + j] <- n))

    grid

//[[8,2,3],[9,6,7],[4,5,1]]
sortMatrix <| array2D [ [ 1; 7; 3 ]; [ 9; 8; 2 ]; [ 4; 5; 6 ] ]

// [[2,1],[1,0]]
sortMatrix <| array2D [ [ 0; 1 ]; [ 1; 2 ] ]

// [[1]]
sortMatrix <| array2D [ [ 1 ] ]
