let shortestPath (grid: int [,]) (k: int) : int =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid
    let moves = [ (1, 0); (0, 1); (-1, 0); (0, -1) ]

    let rec shortestPath' (grid: int [,]) q visited =
        match q with
        | [] -> -1
        | (row, col, eliminations, steps) :: t ->
            if row = rows - 1 && col = cols - 1 then
                steps
            else
                let newCandidates =
                    moves
                    |> List.map (fun (x, y) -> row + x, col + y)
                    |> List.filter (fun (x, y) -> x >= 0 && x < rows && y >= 0 && y < cols)
                    |> List.map (fun (x, y) -> x, y, eliminations - grid.[x, y])
                    |> List.filter (fun (x, y, e) -> e >= 0 && Set.contains (x, y, e) visited |> not)
                    |> List.map (fun (x, y, e) -> x, y, e, steps + 1)

                let newVisited =
                    newCandidates
                    |> List.fold (fun acc (x, y, e, _) -> Set.add (x, y, e) acc) visited

                let newQ = newCandidates @ t
                shortestPath' grid newQ newVisited

    shortestPath' grid [ (0, 0, k, 0) ] Set.empty

let grid1 =
    array2D [ [ 0; 0; 0 ]
              [ 1; 1; 0 ]
              [ 0; 0; 0 ]
              [ 0; 1; 1 ]
              [ 0; 0; 0 ] ]
// 6
shortestPath grid1 1

let grid2 =
    array2D [ [ 0; 1; 1 ]
              [ 1; 1; 1 ]
              [ 1; 0; 0 ] ]
// -1
shortestPath grid2 1
