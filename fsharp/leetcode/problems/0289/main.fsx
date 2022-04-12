let countLives (board: int [,]) (row: int) (col: int) : int =
    let steps =
        [ (-1, -1)
          (-1, 0)
          (-1, 1)
          (0, -1)
          (0, 1)
          (1, -1)
          (1, 0)
          (1, 1) ]

    let rows = Array2D.length1 board
    let cols = Array2D.length2 board

    steps
    |> List.map (fun (r, c) -> row + r, col + c)
    |> List.filter (fun (r, c) -> r >= 0 && r <= rows - 1 && c >= 0 && c <= cols - 1)
    |> List.filter (fun (r, c) -> board.[r, c] = 1)
    |> List.length

let gameOfLife (board: int [,]) : unit =
    let rows = Array2D.length1 board
    let cols = Array2D.length2 board

    let tmp = Array2D.copy board

    for i in 0 .. rows - 1 do
        for j in 0 .. cols - 1 do
            let lives = countLives tmp i j

            if lives < 2 || lives > 3 then
                board.[i, j] <- 0
            elif lives = 3 then
                board.[i, j] <- 1


let board1 =
    array2D [ [ 0; 1; 0 ]
              [ 0; 0; 1 ]
              [ 1; 1; 1 ]
              [ 0; 0; 0 ] ]

// [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
gameOfLife board1
board1

let board2 = array2D [ [ 1; 1 ]; [ 1; 0 ] ]
// [[1,1],[1,1]]
gameOfLife board2
board2
