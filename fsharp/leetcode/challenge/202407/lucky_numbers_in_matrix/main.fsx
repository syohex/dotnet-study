let luckyNumbers (matrix: int[,]) : int list =
    let rows, cols = Array2D.length1 matrix, Array2D.length2 matrix

    let rowMins =
        seq { 0 .. (rows - 1) }
        |> Seq.map (fun i -> Array.min matrix.[i, *])
        |> Seq.toArray

    let colMaxs =
        seq { 0 .. (cols - 1) }
        |> Seq.map (fun i -> Array.max matrix.[*, i])
        |> Seq.toArray

    let rec luckeyNumbers' i j (matrix: int[,]) acc =
        if i >= rows then
            acc
        elif j >= cols then
            luckeyNumbers' (i + 1) 0 matrix acc
        else
            let v = matrix.[i, j]
            let acc = if v = rowMins.[i] && v = colMaxs.[j] then v :: acc else acc
            luckeyNumbers' i (j + 1) matrix acc

    luckeyNumbers' 0 0 matrix []

let matrix1 = array2D [ [ 3; 7; 8 ]; [ 9; 11; 13 ]; [ 15; 16; 17 ] ]
// [15]
luckyNumbers matrix1

let matrix2 = array2D [ [ 1; 10; 4; 2 ]; [ 9; 3; 8; 7 ]; [ 15; 16; 17; 12 ] ]
// [12]
luckyNumbers matrix2

let matrix3 = array2D [ [ 7; 8 ]; [ 1; 2 ] ]
// [7]
luckyNumbers matrix3
