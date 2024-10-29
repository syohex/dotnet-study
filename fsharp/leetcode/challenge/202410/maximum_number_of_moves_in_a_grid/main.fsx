let maxMoves (grid: int[,]) : int =
    let rows, cols = Array2D.length1 grid, Array2D.length2 grid

    let isValid x y =
        x >= 0 && x < rows && y >= 0 && y < cols

    let steps = [ (-1, 1); (0, 1); (1, 1) ]

    let rec maxMoves' q acc visited =
        if Set.isEmpty q then
            acc
        else
            let acc' = q |> Set.fold (fun acc (_, j) -> max acc j) acc

            let q' =
                q
                |> Seq.fold
                    (fun acc (i, j) ->
                        steps
                        |> List.map (fun (x, y) -> i + x, j + y)
                        |> List.filter (fun (x, y) -> isValid x y && not <| Set.contains (x, y) visited)
                        |> List.filter (fun (x, y) -> grid.[i, j] < grid.[x, y])
                        |> List.fold (fun q pos -> Set.add pos q) acc)
                    Set.empty

            let visited' = Set.union visited q
            maxMoves' q' acc' visited'

    let q =
        seq { 0 .. (rows - 1) } |> Seq.fold (fun acc i -> Set.add (i, 0) acc) Set.empty

    maxMoves' q 0 Set.empty

let grid1 =
    array2D [ [ 2; 4; 3; 5 ]; [ 5; 4; 9; 3 ]; [ 3; 4; 2; 11 ]; [ 10; 9; 13; 15 ] ]
// 3
maxMoves grid1

let grid2 = array2D [ [ 3; 2; 4 ]; [ 2; 1; 9 ]; [ 1; 1; 7 ] ]
// 0
maxMoves grid2
