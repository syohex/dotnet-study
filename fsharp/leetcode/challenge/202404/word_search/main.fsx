let exist (board: char[,]) (word: string) : bool =
    let rows, cols = Array2D.length1 board, Array2D.length2 board
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
    let cs = Seq.toList word

    let rec search row col cs visited =
        match cs with
        | [] -> true
        | h :: [] -> h = board.[row, col]
        | h :: t ->
            if h = board.[row, col] then
                let nexts =
                    steps
                    |> List.map (fun (i, j) -> row + i, col + j)
                    |> List.filter (fun (i, j) -> i >= 0 && i < rows && j >= 0 && j < cols)
                    |> List.filter (fun (i, j) -> not <| Set.contains (i, j) visited)

                nexts
                |> List.fold (fun ok (i, j) -> if ok then true else search i j t (Set.add (i, j) visited)) false
            else
                false

    let rec exist' row col =
        if row >= rows then
            false
        elif col >= cols then
            exist' (row + 1) 0
        else if search row col cs (Set.empty |> Set.add (row, col)) then
            true
        else
            exist' row (col + 1)

    exist' 0 0

let board1 =
    array2D [ [ 'A'; 'B'; 'C'; 'E' ]; [ 'S'; 'F'; 'C'; 'S' ]; [ 'A'; 'D'; 'E'; 'E' ] ]
// true
exist board1 "ABCCED"

// true
exist board1 "SEE"

// false
exist board1 "ABCB"

let board2 = array2D [ [ 'a'; 'b' ]; [ 'c'; 'd' ] ]
// true
exist board2 "acdb"

let board3 = array2D [ [ 'a'; 'a' ] ]
// false
exist board3 "aaa"
