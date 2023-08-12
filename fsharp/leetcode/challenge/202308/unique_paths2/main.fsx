let uniquePathsWithObstacles (obstacleGrid: int[,]) : int =
    let rows, cols = Array2D.length1 obstacleGrid, Array2D.length2 obstacleGrid
    let steps = [ (1, 0); (0, 1) ]

    let rec uniquePathsWithObstacles' q (dp: int[,]) =
        if Set.isEmpty q then
            dp.[rows - 1, cols - 1]
        else
            let q' =
                q
                |> Seq.fold
                    (fun acc (r, c) ->
                        steps
                        |> List.map (fun (x, y) -> r + x, c + y)
                        |> List.filter (fun (x, y) -> x < rows && y < cols && obstacleGrid.[x, y] = 0)
                        |> List.fold
                            (fun acc' (x, y) ->
                                dp.[x, y] <- dp.[x, y] + dp.[r, c]
                                Set.add (x, y) acc')
                            acc)
                    Set.empty

            uniquePathsWithObstacles' q' dp

    if obstacleGrid.[0, 0] = 0 then
        let q = Set.empty |> Set.add (0, 0)
        let dp = Array2D.zeroCreate rows cols
        dp.[0, 0] <- 1
        uniquePathsWithObstacles' q dp
    else
        0

let grid1 = array2D [ [ 0; 0; 0 ]; [ 0; 1; 0 ]; [ 0; 0; 0 ] ]
// 2
uniquePathsWithObstacles grid1

let grid2 = array2D [ [ 0; 1 ]; [ 0; 0 ] ]
// 1
uniquePathsWithObstacles grid2

let grid3 = array2D [ [ 1 ] ]
// 0
uniquePathsWithObstacles grid3

let grid4 =
    array2D
        [ [ 0; 0; 0; 0 ]
          [ 0; 1; 0; 0 ]
          [ 0; 0; 0; 0 ]
          [ 0; 0; 1; 0 ]
          [ 0; 0; 0; 0 ] ]
// 7
uniquePathsWithObstacles grid4
