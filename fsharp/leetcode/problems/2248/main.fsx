let intersection (nums: int list list) : int list =
    nums
    |> List.map Set.ofList
    |> List.reduce (fun a b -> Set.intersect a b)
    |> Set.toList
    |> List.sort

// [3;4]
intersection [ [ 3; 1; 2; 4; 5 ]
               [ 1; 2; 3; 4 ]
               [ 3; 4; 5; 6 ] ]

// []
intersection [ [ 1; 2; 3 ]
               [ 4; 5; 6 ] ]
