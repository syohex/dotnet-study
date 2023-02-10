let maxDistance (grid: int[,]) : int =
    let n = Array2D.length1 grid
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec maxDistance' q distance (visited: int[,]) =
        match q with
        | [] -> distance
        | _ ->
            let q' =
                q
                |> List.collect (fun (r, c) -> steps |> List.map (fun (x, y) -> r + x, c + y))
                |> List.filter (fun (r, c) -> r >= 0 && r < n && c >= 0 && c < n && visited.[r, c] = 0)

            q' |> List.iter (fun (r, c) -> visited.[r, c] <- 1)
            maxDistance' q' (distance + 1) visited


    let mutable q = []
    let visited = Array2D.init n n (fun i j -> grid.[i, j])

    for i in 0 .. (n - 1) do
        for j in 0 .. (n - 1) do
            if grid.[i, j] = 1 then
                q <- (i, j) :: q

    let lands = List.length q

    if lands = 0 || lands = n * n then
        -1
    else
        maxDistance' q -1 visited

let grid1 = array2D [ [ 1; 0; 1 ]; [ 0; 0; 0 ]; [ 1; 0; 1 ] ]
// 2
maxDistance grid1

let grid2 = array2D [ [ 1; 0; 0 ]; [ 0; 0; 0 ]; [ 0; 0; 0 ] ]
// 4
maxDistance grid2

let grid3 = array2D [ [ 1; 1 ]; [ 1; 1 ] ]
// -1
maxDistance grid3
