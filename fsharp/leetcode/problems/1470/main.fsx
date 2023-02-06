let shuffle (nums: int list) (n: int) : int list =
    (List.take n nums, List.skip n nums)
    ||> List.zip
    |> List.fold (fun acc (x, y) -> y :: x :: acc) []
    |> List.rev

// [2;3;5;4;1;7]
shuffle [ 2; 5; 1; 3; 4; 7 ] 3

// [1;4;2;3;3;2;4;1]
shuffle [ 1; 2; 3; 4; 4; 3; 2; 1 ] 4

// [1;2;1;2]
shuffle [ 1; 1; 2; 2 ] 2
