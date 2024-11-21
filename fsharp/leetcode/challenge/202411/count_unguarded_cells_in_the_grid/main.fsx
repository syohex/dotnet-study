type State =
    | Unguarded
    | Guard
    | Wall
    | Guarded

let countUnguarded (m: int) (n: int) (guards: (int * int) list) (walls: (int * int) list) : int =
    let rec setGuarded (row, col) (x, y) (matrix: State[,]) =
        if row >= 0 && row < m && col >= 0 && col < n then
            match matrix.[row, col] with
            | Wall
            | Guard -> ()
            | _ ->
                matrix.[row, col] <- Guarded
                setGuarded (row + x, col + y) (x, y) matrix

    let matrix = Array2D.init m n (fun _ _ -> Unguarded)
    guards |> List.iter (fun (i, j) -> matrix.[i, j] <- Guard)
    walls |> List.iter (fun (i, j) -> matrix.[i, j] <- Wall)

    let directions = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    guards
    |> List.iter (fun (row, col) ->
        directions
        |> List.iter (fun (x, y) -> setGuarded (row + x, col + y) (x, y) matrix))

    matrix |> Seq.cast<State> |> Seq.filter ((=) Unguarded) |> Seq.length

// 7
countUnguarded 4 6 [ (0, 0); (1, 1); (2, 3) ] [ (0, 1); (2, 2); (1, 4) ]
// 4
countUnguarded 3 3 [ (1, 1) ] [ (0, 1); (1, 0); (2, 1); (1, 2) ]
// 1
countUnguarded 2 7 [ (1, 5); (1, 1); (1, 6); (0, 2) ] [ (0, 6); (0, 3); (0, 5) ]
