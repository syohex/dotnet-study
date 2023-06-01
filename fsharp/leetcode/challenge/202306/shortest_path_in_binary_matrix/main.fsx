let shortestPathBianryMatrix (grid: int[,]) : int =
    let steps = [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
    let n = Array2D.length1 grid

    let check q =
        List.exists (fun (x, y) -> x = n - 1 && y = n - 1) q

    let rec shortestPathBianryMatrix' q count =
        match q with
        | [] -> -1
        | _ ->
            if check q then
                count
            else
                let q' =
                    q
                    |> List.fold
                        (fun acc (x, y) ->
                            let nexts = List.map (fun (a, b) -> x + a, y + b) steps
                            nexts @ acc)
                        []
                    |> List.filter (fun (x, y) -> x >= 0 && x < n && y >= 0 && y < n && grid.[x, y] = 0)

                List.iter (fun (x, y) -> grid.[x, y] <- 1) q'
                shortestPathBianryMatrix' q' (count + 1)

    if grid.[0, 0] = 1 then
        -1
    else
        shortestPathBianryMatrix' [ (0, 0) ] 1

let grid1 = array2D [ [ 0; 1 ]; [ 1; 0 ] ]
// 2
shortestPathBianryMatrix grid1

let grid2 = array2D [ [ 0; 0; 0 ]; [ 1; 1; 0 ]; [ 1; 1; 0 ] ]
// 4
shortestPathBianryMatrix grid2

let grid3 = array2D [ [ 1; 0; 0 ]; [ 1; 1; 0 ]; [ 1; 1; 0 ] ]
// -1
shortestPathBianryMatrix grid3

// 1
shortestPathBianryMatrix (array2D [ [ 0 ] ])

// -1
shortestPathBianryMatrix (array2D [ [ 1 ] ])
