let numSpecial (mat: int[,]) : int =
    let rows, cols = Array2D.length1 mat, Array2D.length2 mat

    let isSpecial row col =
        if mat.[row, col] = 1 then
            let rowOnes =
                seq { 0 .. (rows - 1) } |> Seq.filter (fun x -> mat.[x, col] = 1) |> Seq.length

            let colOnes =
                seq { 0 .. (cols - 1) } |> Seq.filter (fun y -> mat.[row, y] = 1) |> Seq.length

            rowOnes = 1 && colOnes = 1
        else
            false

    let rec numSpecial' row col acc =
        if row >= rows then
            acc
        elif col >= cols then
            numSpecial' (row + 1) 0 acc
        else
            let acc' = if isSpecial row col then acc + 1 else acc
            numSpecial' row (col + 1) acc'

    numSpecial' 0 0 0

let mat1 = array2D [ [ 1; 0; 0 ]; [ 0; 0; 1 ]; [ 1; 0; 0 ] ]
// 1
numSpecial mat1

let mat2 = array2D [ [ 1; 0; 0 ]; [ 0; 1; 0 ]; [ 0; 0; 1 ] ]
// 3
numSpecial mat2
