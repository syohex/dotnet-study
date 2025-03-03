let pivotArray (nums: int list) (pivot: int) : int list =
    nums
    |> List.fold
        (fun (acc1, acc2, acc3) n ->
            if n < pivot then n :: acc1, acc2, acc3
            elif n = pivot then acc1, n :: acc2, acc3
            else acc1, acc2, n :: acc3)
        ([], [], [])
    |> fun (acc1, acc2, acc3) -> acc3 @ acc2 @ acc1
    |> List.rev

// [9,5,3,10,10,12,14]
pivotArray [ 9; 12; 5; 10; 14; 3; 10 ] 10

// [-3,2,4,3]
pivotArray [ -3; 4; 3; 2 ] 2
