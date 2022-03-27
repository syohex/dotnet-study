let kWeakestRows (mat: int list list) (k: int) : int list =
    mat
    |> List.mapi (fun i v ->
        let count = v |> List.reduce (+)
        i, count)
    |> List.sortWith (fun (i, a) (j, b) ->
        if a <> b then
            compare a b
        else
            compare i j)
    |> List.take k
    |> List.map fst

// [2,0,3]
let mat1 =
    [ [ 1; 1; 0; 0; 0 ]
      [ 1; 1; 1; 1; 0 ]
      [ 1; 0; 0; 0; 0 ]
      [ 1; 1; 0; 0; 0 ]
      [ 1; 1; 1; 1; 1 ] ]

kWeakestRows mat1 3

// [0,2]
let mat2 =
    [ [ 1; 0; 0; 0 ]
      [ 1; 1; 1; 1 ]
      [ 1; 0; 0; 0 ]
      [ 1; 0; 0; 0 ] ]

kWeakestRows mat2 2
