let lastStoneWeight (stones: int list) : int =
    let rec lastStoneWeight' stones =
        match stones with
        | [] -> 0
        | h :: [] -> h
        | h1 :: h2 :: t ->
            if h1 = h2 then
                lastStoneWeight' t
            else
                let stones' = ((h1 - h2) :: t) |> List.sort |> List.rev
                lastStoneWeight' stones'

    lastStoneWeight' (stones |> List.sort |> List.rev)

// 1
lastStoneWeight [ 2; 7; 4; 1; 8; 1 ]

// 1
lastStoneWeight [ 1 ]
