let rotateTheBox (box: char[,]) : char[,] =
    let rows, cols = Array2D.length1 box, Array2D.length2 box

    let rec findPosition row col limit (box: char[,]) lastEmpty =
        if col >= limit then
            lastEmpty
        else
            match box.[row, col] with
            | '*'
            | '#' -> lastEmpty
            | _ -> findPosition row (col + 1) limit box col

    for i in 0 .. (rows - 1) do
        let mutable limit = cols

        for j in seq { 0 .. (cols - 1) } |> Seq.rev do
            if box.[i, j] = '#' then
                let pos = findPosition i (j + 1) limit box j
                box.[i, j] <- ' '
                box.[i, pos] <- '#'
                limit <- pos

    let ret: char[,] = Array2D.zeroCreate cols rows

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            ret.[j, rows - 1 - i] <- box.[i, j]

    ret

let box1 = array2D [ [ '#'; '.'; '#' ] ]
// [["."],
//  ["#"],
//  ["#"]]
rotateTheBox box1

let box2 = array2D [ [ '#'; '.'; '*'; '.' ]; [ '#'; '#'; '*'; '.' ] ]
// [["#","."],
//  ["#","#"],
//  ["*","*"],
//  [".","."]]
rotateTheBox box2

let box3 =
    array2D
        [ [ '#'; '#'; '*'; '.'; '*'; '.' ]
          [ '#'; '#'; '#'; '*'; '.'; '.' ]
          [ '#'; '#'; '#'; '.'; '#'; '.' ] ]
// [[".","#","#"],
//  [".","#","#"],
//  ["#","#","*"],
//  ["#","*","."],
//  ["#",".","*"],
//  ["#",".","."]]
rotateTheBox box3
