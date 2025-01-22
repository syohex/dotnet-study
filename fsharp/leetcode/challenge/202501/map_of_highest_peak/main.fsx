let highestPeak (isWater: int[,]) : int[,] =
    let rows, cols = Array2D.length1 isWater, Array2D.length2 isWater
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec highestPeak' q height visited (acc: int[,]) =
        if Set.isEmpty q then
            acc
        else
            q |> Set.iter (fun (x, y) -> acc.[x, y] <- height)

            let nexts =
                q
                |> Set.fold
                    (fun acc (row, col) ->
                        steps
                        |> List.map (fun (x, y) -> row + x, col + y)
                        |> List.filter (fun (x, y) ->
                            x >= 0 && x < rows && y >= 0 && y < cols && (not <| Set.contains (x, y) visited))
                        |> List.fold (fun acc (x, y) -> Set.add (x, y) acc) acc)
                    Set.empty

            let visited = nexts |> Set.fold (fun acc (x, y) -> Set.add (x, y) acc) visited
            highestPeak' nexts (height + 1) visited acc

    let q =
        seq {
            for i in 0 .. rows - 1 do
                for j in 0 .. cols - 1 do
                    if isWater.[i, j] = 1 then
                        yield i, j
        }
        |> Set.ofSeq

    let visited = q |> Set.fold (fun acc (x, y) -> Set.add (x, y) acc) Set.empty
    let ret = Array2D.zeroCreate rows cols
    highestPeak' q 0 visited ret

let isWater1 = array2D [ [ 0; 1 ]; [ 0; 0 ] ]
// [[1,0],[2,1]]
highestPeak isWater1

let isWater2 = array2D [ [ 0; 0; 1 ]; [ 1; 0; 0 ]; [ 0; 0; 0 ] ]
// [[1,1,0];[0,1,1];[1;2;2]]
highestPeak isWater2
