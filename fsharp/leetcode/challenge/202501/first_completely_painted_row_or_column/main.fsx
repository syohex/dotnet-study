let firstCompleteIndex (arr: int list) (mat: int[,]) : int =
    let rows, cols = Array2D.length1 mat, Array2D.length2 mat

    let rec firstCompleteIndex' arr i (rowSum: int[]) (colSum: int[]) valueMap =
        match arr with
        | [] -> failwith "never reach here"
        | h :: t ->
            let row, col = Map.find h valueMap
            rowSum.[row] <- rowSum.[row] + 1
            colSum.[col] <- colSum.[col] + 1

            if rowSum.[row] = cols || colSum.[col] = rows then
                i
            else
                firstCompleteIndex' t (i + 1) rowSum colSum valueMap

    let valueMap =
        seq {
            for i in 0 .. (rows - 1) do
                for j in 0 .. (cols - 1) do
                    yield i, j, mat.[i, j]
        }
        |> Seq.fold (fun acc (i, j, v) -> Map.add v (i, j) acc) Map.empty

    let rowSum, colSum = Array.zeroCreate rows, Array.zeroCreate cols
    firstCompleteIndex' arr 0 rowSum colSum valueMap

let mat1 = array2D [ [ 1; 4 ]; [ 2; 3 ] ]
let mat2 = array2D [ [ 3; 2; 5 ]; [ 1; 4; 6 ]; [ 8; 7; 9 ] ]
let mat3 = array2D [ [ 4; 3; 5 ]; [ 1; 2; 6 ] ]

// 2
firstCompleteIndex [ 1; 3; 4; 2 ] mat1

// 3
firstCompleteIndex [ 2; 8; 7; 4; 1; 3; 5; 6; 9 ] mat2

// 1
firstCompleteIndex [ 1; 4; 5; 2; 6; 3 ] mat3
