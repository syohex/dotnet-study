let maxEqualRowsAfterFlips (matrix: int[,]) : int =
    let limit = (Array2D.length1 matrix - 1)

    let rec maxEqualRowsAfterFlips' i acc =
        if i > limit then
            acc
        else
            let v = matrix.[i, *]
            let inverted = v |> Array.map (fun n -> if n = 1 then 0 else 1)

            let ret =
                seq { (i + 1) .. limit }
                |> Seq.fold
                    (fun acc j ->
                        let v' = matrix.[j, *]
                        if v' = v || v' = inverted then acc + 1 else acc)
                    1

            maxEqualRowsAfterFlips' (i + 1) (max acc ret)

    maxEqualRowsAfterFlips' 0 1

// 1
maxEqualRowsAfterFlips (array2D [ [ 0; 1 ]; [ 1; 1 ] ])

// 2
maxEqualRowsAfterFlips (array2D [ [ 0; 1 ]; [ 1; 0 ] ])

// 2
maxEqualRowsAfterFlips (array2D [ [ 0; 0; 0 ]; [ 0; 0; 1 ]; [ 1; 1; 0 ] ])
