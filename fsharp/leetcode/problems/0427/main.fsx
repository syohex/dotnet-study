type QuadTree =
    | Leaf
    | Node of int * bool * QuadTree * QuadTree * QuadTree * QuadTree

let construct (grid: int[,]) : QuadTree =
    let check n x y =
        let mutable v = grid.[x, y]

        for i in 0 .. (n - 1) do
            for j in 0 .. (n - 1) do
                if v <> -1 && v <> grid.[x + i, y + j] then
                    v <- -1

        v

    let rec construct' n x y =
        let v = check n x y

        if v <> -1 then
            Node(v, true, Leaf, Leaf, Leaf, Leaf)
        else
            let n' = n / 2
            let topLeft = construct' n' x y
            let topRight = construct' n' x (y + n')
            let bottomLeft = construct' n' (x + n') y
            let bottomRight = construct' n' (x + n') (y + n')

            Node(1, false, topLeft, topRight, bottomLeft, bottomRight)

    construct' (Array2D.length1 grid) 0 0

let grid1 = array2D [ [ 0; 1 ]; [ 1; 0 ] ]
// [[0,1],[1,0],[1,1],[1,1],[1,0]]
construct grid1

let grid2 =
    array2D
        [ [ 1; 1; 1; 1; 0; 0; 0; 0 ]
          [ 1; 1; 1; 1; 0; 0; 0; 0 ]
          [ 1; 1; 1; 1; 1; 1; 1; 1 ]
          [ 1; 1; 1; 1; 1; 1; 1; 1 ]
          [ 1; 1; 1; 1; 0; 0; 0; 0 ]
          [ 1; 1; 1; 1; 0; 0; 0; 0 ]
          [ 1; 1; 1; 1; 0; 0; 0; 0 ]
          [ 1; 1; 1; 1; 0; 0; 0; 0 ] ]

// [[0,1],[1,1],[0,1],[1,1],[1,0],null,null,null,null,[1,0],[1,0],[1,1],[1,1]]
construct grid2
