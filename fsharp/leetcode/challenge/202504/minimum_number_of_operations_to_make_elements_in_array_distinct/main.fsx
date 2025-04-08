let minimumOperations (nums: int list) : int =
    let rec minimumOperations' nums s =
        match nums with
        | [] -> 0
        | (i, h) :: t ->
            if Set.contains h s then
                i / 3 + 1
            else
                minimumOperations' t (Set.add h s)

    minimumOperations' (List.indexed nums) Set.empty

// 2
minimumOperations [ 1; 2; 3; 4; 2; 3; 3; 5; 7 ]

// 2
minimumOperations [ 4; 5; 6; 4; 4 ]

// 0
minimumOperations [ 6; 7; 8; 9 ]
