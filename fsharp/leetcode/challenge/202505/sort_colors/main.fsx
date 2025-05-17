let sortColors (nums: int[]) : int[] =
    nums
    |> Array.countBy id
    |> Array.sortBy fst
    |> Array.fold (fun acc (n, count) -> Array.append acc <| Array.init count (fun _ -> n)) Array.empty

// [2]
sortColors [| 2 |]

// [0,0,1,1,2,2]
sortColors [| 2; 0; 2; 1; 1; 0 |]

// [0,1,2]
sortColors [| 2; 0; 1 |]
