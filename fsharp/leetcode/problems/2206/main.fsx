let divideArray (nums: int list) : bool =
    nums
    |> List.countBy id
    |> List.forall (fun (_, count) -> count % 2 = 0)

// true
divideArray [ 3; 2; 3; 2; 2; 2 ]

// false
divideArray [ 1; 2; 3; 4 ]
