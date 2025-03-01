let applyOperations (nums: int list) : int list =
    let rec applyOperations' nums zeros acc =
        match nums with
        | [] -> seq { 1..zeros } |> Seq.fold (fun acc _ -> 0 :: acc) acc |> List.rev
        | h :: [] ->
            if h = 0 then
                applyOperations' [] (zeros + 1) acc
            else
                applyOperations' [] zeros (h :: acc)
        | h :: t when h = 0 -> applyOperations' t (zeros + 1) acc
        | h1 :: h2 :: t ->
            if h1 = h2 then
                applyOperations' t (zeros + 1) ((h1 * 2) :: acc)
            else
                applyOperations' (List.tail nums) zeros (h1 :: acc)

    applyOperations' nums 0 []

// [1,4,2,0,0,0]
applyOperations [ 1; 2; 2; 1; 1; 0 ]

// [1;0]
applyOperations [ 0; 1 ]
