let kWeakestRows (mat: int list list) (k: int) : int list =
    mat
    |> List.map List.sum
    |> List.indexed
    |> List.sortBy snd
    |> List.take k
    |> List.map fst

let mat1 =
    [ [ 1; 1; 0; 0; 0 ]
      [ 1; 1; 1; 1; 0 ]
      [ 1; 0; 0; 0; 0 ]
      [ 1; 1; 0; 0; 0 ]
      [ 1; 1; 1; 1; 1 ] ]
// [2,0,3]
kWeakestRows mat1 3

let mat2 = [ [ 1; 0; 0; 0 ]; [ 1; 1; 1; 1 ]; [ 1; 0; 0; 0 ]; [ 1; 0; 0; 0 ] ]
// [0,2]
kWeakestRows mat2 2
