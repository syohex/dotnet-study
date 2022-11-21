let nearestExit (maze: char [,]) ((row, col): int * int) : int =
    let rows = Array2D.length1 maze
    let cols = Array2D.length2 maze

    let rec nearestExit' (q: (int * int * int) list) (maze: char [,]) =
        match q with
        | [] -> -1
        | _ ->
            let rev = List.rev q
            let (row, col, step) = rev |> List.head

            if row < 0 || row >= rows || col < 0 || col >= cols then
                if step = 1 then
                    nearestExit' (List.tail rev) maze
                else
                    step - 1
            else
                let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

                let q' =
                    steps
                    |> List.fold
                        (fun acc (x, y) ->
                            let newX, newY = row + x, col + y

                            if newX < 0
                               || newX >= rows
                               || newY < 0
                               || newY >= cols then
                                (newX, newY, step + 1) :: acc
                            elif newX >= 0
                                 && newX < rows
                                 && newY >= 0
                                 && newY <= cols
                                 && maze.[newX, newY] = '.' then
                                maze.[newX, newY] <- '+'
                                (newX, newY, step + 1) :: acc
                            else
                                acc)
                        (rev |> List.tail)

                nearestExit' q' maze

    maze.[row, col] <- '+'
    nearestExit' [ (row, col, 0) ] maze


let maze1 =
    array2D [ [ '+'; '+'; '.'; '+' ]
              [ '.'; '.'; '.'; '+' ]
              [ '+'; '+'; '+'; '.' ] ]

let entrance1 = (1, 2)
// 1
nearestExit maze1 entrance1

let maze2 =
    array2D [ [ '+'; '+'; '+' ]
              [ '.'; '.'; '.' ]
              [ '+'; '+'; '+' ] ]

let entrance2 = (1, 0)
// 2
nearestExit maze2 entrance2

let maze3 = array2D [ [ '.'; '+' ] ]
let entrance3 = (0, 0)
// -1
nearestExit maze3 entrance3

let maze4 =
    array2D [ [ '+'; '.'; '+'; '+'; '+'; '+'; '+' ]
              [ '+'; '.'; '+'; '.'; '.'; '.'; '+' ]
              [ '+'; '.'; '+'; '.'; '+'; '.'; '+' ]
              [ '+'; '.'; '.'; '.'; '+'; '.'; '+' ]
              [ '+'; '+'; '+'; '+'; '+'; '.'; '+' ] ]

let entrance4 = (0, 1)
// 12
nearestExit maze4 entrance4
