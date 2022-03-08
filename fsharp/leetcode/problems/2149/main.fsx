let rearrangeArray (nums: int list) : int list =
    (nums |> List.filter (fun n -> n >= 0), nums |> List.filter (fun n -> n < 0))
    ||> List.zip
    |> List.fold (fun acc (n1, n2) -> n2 :: n1 :: acc) []
    |> List.rev


//  [3,-2,1,-5,2,-4]
rearrangeArray [ 3; 1; -2; -5; 2; -4 ]

// [1, -1]
rearrangeArray [ -1; 1 ]
