#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let trapRainWater (heightMap: int[,]) : int =
    let rows, cols = Array2D.length1 heightMap, Array2D.length2 heightMap
    let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec trapRainWater' q visited acc =
        match PriorityQueue.tryPop q with
        | None -> acc
        | Some(((height, row, col), q')) ->
            let nexts =
                steps
                |> List.map (fun (x, y) -> row + x, col + y)
                |> List.filter (fun (x, y) ->
                    x >= 0 && x < rows && y >= 0 && y < cols && (not <| Set.contains (x, y) visited))

            let visited = nexts |> List.fold (fun acc (x, y) -> Set.add (x, y) acc) visited

            let (q', acc) =
                nexts
                |> List.fold
                    (fun (q, acc) (x, y) ->
                        let h = heightMap.[x, y]
                        let acc = if h < height then acc + height - h else acc
                        PriorityQueue.insert (max h height, x, y) q, acc)
                    (q', acc)

            trapRainWater' q' visited acc

    let rec initQueue row q visited =
        if row >= rows then
            q, visited
        elif row = 0 || row = rows - 1 then
            let q, visited =
                seq { 0 .. (cols - 1) }
                |> Seq.fold
                    (fun (q, visited) i ->
                        PriorityQueue.insert (heightMap.[row, i], row, i) q, Set.add (row, i) visited)
                    (q, visited)

            initQueue (row + 1) q visited
        else
            let q, visited =
                PriorityQueue.insert (heightMap.[row, 0], row, 0) q, Set.add (row, 0) visited

            let q, visited =
                PriorityQueue.insert (heightMap.[row, cols - 1], row, cols - 1) q, Set.add (row, cols - 1) visited

            initQueue (row + 1) q visited

    let q, visited = initQueue 0 (PriorityQueue.empty false) Set.empty
    trapRainWater' q visited 0

let heightMap1 =
    array2D [ [ 1; 4; 3; 1; 3; 2 ]; [ 3; 2; 1; 3; 2; 4 ]; [ 2; 3; 3; 2; 3; 1 ] ]
// 4
trapRainWater heightMap1

let heightMap2 =
    array2D
        [ [ 3; 3; 3; 3; 3 ]
          [ 3; 2; 2; 2; 3 ]
          [ 3; 2; 1; 2; 3 ]
          [ 3; 2; 2; 2; 3 ]
          [ 3; 3; 3; 3; 3 ] ]
// 10
trapRainWater heightMap2
