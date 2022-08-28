let diagonalSort (mat: int [,]) : int [,] =
    let mutable m: Map<int, int list> = Map.empty

    let rows = Array2D.length1 mat
    let cols = Array2D.length2 mat

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            let key = i - j

            match Map.tryFind key m with
            | None -> m <- Map.add key [ mat[i, j] ] m
            | Some (v) ->
                let v' = (mat[i, j] :: v) |> List.sort
                m <- Map.add key v' m

    for i in 0 .. (rows - 1) do
        for j in 0 .. (cols - 1) do
            let key = i - j

            match Map.find key m with
            | [] -> failwith "never reach here"
            | h :: t ->
                mat.[i, j] <- h
                m <- Map.add key t m

    mat

let mat1 =
    array2D [ [ 3; 3; 1; 1 ]
              [ 2; 2; 1; 2 ]
              [ 1; 1; 1; 2 ] ]

// [[1,1,1,1],[1,2,2,2],[1,2,3,3]]
diagonalSort mat1

let mat2 =
    array2D [ [ 11; 25; 66; 1; 69; 7 ]
              [ 23; 55; 17; 45; 15; 52 ]
              [ 75; 31; 36; 44; 58; 8 ]
              [ 22; 27; 33; 25; 68; 4 ]
              [ 84; 28; 14; 11; 5; 50 ] ]

// [[5,17,4,1,52,7],[11,11,25,45,8,69],[14,23,25,44,58,15],[22,27,31,36,50,66],[84,28,75,33,55,68]]
diagonalSort mat2
