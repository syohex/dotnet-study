let updateMatrix (mat: int[,]) : int[,] =
    let rows, cols = Array2D.length1 mat, Array2D.length2 mat
    let moves = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let rec updateMatrix' q steps visited (ret: int[,]) =
        match q with
        | [] -> ret
        | _ ->
            q
            |> List.iter (fun (row, col) ->
                if ret.[row, col] = -1 then
                    ret.[row, col] <- steps)

            let q' =
                q
                |> List.fold
                    (fun acc (row, col) ->
                        let nexts = moves |> List.map (fun (x, y) -> row + x, col + y)
                        nexts @ acc)
                    []
                |> List.filter (fun (row, col) -> row >= 0 && row < rows && col >= 0 && col < cols)
                |> List.filter (fun pos -> Set.contains pos visited |> not)
                |> Set.ofList
                |> Set.toList

            let visited' = q' |> List.fold (fun acc pos -> Set.add pos acc) visited
            updateMatrix' q' (steps + 1) visited' ret


    let indexes = Array2D.mapi (fun i j _ -> i, j) mat |> Seq.cast<(int * int)>

    let ret = Array2D.init rows cols (fun _ _ -> -1)

    let q, visited =
        indexes
        |> Seq.fold
            (fun (q, visited) (i, j) ->
                if mat.[i, j] = 0 then
                    ret.[i, j] <- 0
                    (i, j) :: q, Set.add (i, j) visited
                else
                    q, visited)
            ([], Set.empty)

    updateMatrix' q 0 visited ret

let mat1 = array2D [ [ 0; 0; 0 ]; [ 0; 1; 0 ]; [ 0; 0; 0 ] ]
//  [[0;0;0];[0;1;0];[0;0;0]]
updateMatrix mat1

let mat2 = array2D [ [ 0; 0; 0 ]; [ 0; 1; 0 ]; [ 1; 1; 1 ] ]
//  [[0;0;0];[0;1;0];[1;2;1]]
updateMatrix mat2

let mat3 =
    array2D
        [ [ 0; 1; 0; 1; 1 ]
          [ 1; 1; 0; 0; 1 ]
          [ 0; 0; 0; 1; 0 ]
          [ 1; 0; 1; 1; 1 ]
          [ 1; 0; 0; 0; 1 ] ]
// [[0;1;0;1;2];[1;1;0;0;1];[0;0;0;1;0];[1;0;1;1;1];[1;0;0;0;1]]
updateMatrix mat3
