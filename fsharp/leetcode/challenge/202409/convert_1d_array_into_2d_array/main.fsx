let construct2DArray (original: int list) (m: int) (n: int) : int[,] =
    let rec construct2DArray' original (acc: int[,]) =
        match original with
        | [] -> acc
        | (i, h) :: t ->
            acc.[i / n, i % n] <- h
            construct2DArray' t acc

    if List.length original <> m * n then
        Array2D.zeroCreate 0 0
    else
        let acc = Array2D.zeroCreate m n
        construct2DArray' (List.indexed original) acc

// [[1,2],[3,4]]
construct2DArray [ 1; 2; 3; 4 ] 2 2

// [[1,2,3]]
construct2DArray [ 1; 2; 3 ] 1 3

// [[]]
construct2DArray [ 1; 2 ] 1 1
