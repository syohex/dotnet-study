let rearrangeArray (nums: int list) : int list =
    List.zip (List.filter (fun n -> n >= 0) nums) (List.filter (fun n -> n < 0) nums)
    |> List.fold (fun acc (p, n) -> n :: p :: acc) []
    |> List.rev

// [3,-2,1,-5,2,-4]
rearrangeArray [ 3; 1; -2; -5; 2; -4 ]

// [1;-1]
rearrangeArray [ -1; 1 ]
