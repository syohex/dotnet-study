let rec findFinalValue (nums: int list) (original: int) : int =
    nums
    |> List.sort
    |> List.fold (fun acc n -> if acc = n then acc * 2 else acc) original

// 24
findFinalValue [ 5; 3; 6; 1; 12 ] 3

// 4
findFinalValue [ 2; 7; 9 ] 4

// 16
findFinalValue [ 8; 19; 4; 2; 15; 3 ] 2
