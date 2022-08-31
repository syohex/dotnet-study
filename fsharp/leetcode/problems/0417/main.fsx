let pacificAtlantic (heights: int [,]) : (int * int) list =
    let rows = Array2D.length1 heights
    let cols = Array2D.length2 heights

    let rec collectReachablePoints candidates visited acc =
        match candidates with
        | [] -> acc
        | (x, y) :: rest ->
            let steps = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]
            let current = heights.[x, y]

            let visited' = Set.add (x, y) visited

            let newCandidates =
                steps
                |> List.map (fun (i, j) -> x + i, y + j)
                |> List.filter (fun (x, y) -> x >= 0 && x < rows && y >= 0 && y < cols)
                |> List.filter (fun (x, y) -> Set.contains (x, y) visited' |> not)
                |> List.filter (fun (x, y) -> current <= heights.[x, y])

            let candidates' = newCandidates @ rest
            let acc' = newCandidates @ acc
            collectReachablePoints candidates' visited' acc'

    let p1 =
        seq { 0 .. (cols - 1) }
        |> Seq.map (fun i -> 0, i)
        |> Seq.toList

    let p2 =
        seq { 1 .. (rows - 1) }
        |> Seq.map (fun i -> i, 0)
        |> Seq.toList

    let pacificPoints = p1 @ p2

    let a1 =
        seq { 0 .. (cols - 1) }
        |> Seq.map (fun i -> rows - 1, i)
        |> Seq.toList

    let a2 =
        seq { 0 .. (rows - 2) }
        |> Seq.map (fun i -> i, cols - 1)
        |> Seq.toList

    let atlanticPoints = a1 @ a2

    let p3 =
        collectReachablePoints pacificPoints (Set.ofList pacificPoints) pacificPoints

    let a3 =
        collectReachablePoints atlanticPoints (Set.ofList atlanticPoints) atlanticPoints

    p3
    |> List.filter (fun point1 ->
        List.tryFind (fun point2 -> point1 = point2) a3
        |> Option.isSome)
    |> List.sortWith (fun (x1, y1) (x2, y2) ->
        if x1 = x2 then
            compare y1 y2
        else
            compare x1 x2)

let height1 =
    array2D [ [ 1; 2; 2; 3; 5 ]
              [ 3; 2; 3; 4; 4 ]
              [ 2; 4; 5; 3; 1 ]
              [ 6; 7; 1; 4; 5 ]
              [ 5; 1; 1; 2; 4 ] ]

// [[0,4],[1,3],[1,4],[2,2],[3,0],[3,1],[4,0]]
pacificAtlantic height1

// [[0, 0]]
pacificAtlantic (array2D [ [ 1 ] ])
