let uniquePaths3 (grid: int [,]) : int =
    let mutable len = 0
    let mutable start = 0, 0
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            if grid.[i, j] = 1 then start <- i, j
            if grid.[i, j] <> -1 then len <- len + 1

    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec uniquePaths3' q (grid: int [,]) acc =
        match q with
        | [] -> acc
        | ((row, col), visited) :: t ->
            if grid.[row, col] = 2 then
                if (visited |> Set.toList |> List.length) = len then
                    uniquePaths3' t grid (acc + 1)
                else
                    uniquePaths3' t grid acc
            else
                let q' =
                    steps
                    |> List.map (fun (r, c) -> row + r, col + c)
                    |> List.filter (fun (r, c) ->
                        r >= 0
                        && r < rows
                        && c >= 0
                        && c < cols
                        && grid.[r, c] <> -1
                        && (Set.contains (r, c) visited |> not))
                    |> List.fold (fun acc pos -> (pos, Set.add pos visited) :: acc) t

                uniquePaths3' q' grid acc

    let q = [ (start, Set.add start Set.empty) ]
    uniquePaths3' q grid 0

let grid1 =
    array2D [ [ 1; 0; 0; 0 ]
              [ 0; 0; 0; 0 ]
              [ 0; 0; 2; -1 ] ]
// 2
uniquePaths3 grid1

let grid2 =
    array2D [ [ 1; 0; 0; 0 ]
              [ 0; 0; 0; 0 ]
              [ 0; 0; 0; 2 ] ]
// 4
uniquePaths3 grid2

let grid3 = array2D [ [ 0; 1 ]; [ 2; 0 ] ]
// 0
uniquePaths3 grid3
