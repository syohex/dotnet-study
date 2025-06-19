let partitionArray (nums: int list) (k: int) : int =
    let rec partitionArray' nums minVal acc =
        match nums with
        | [] -> acc
        | h :: t ->
            if h - minVal > k then
                partitionArray' t h (acc + 1)
            else
                partitionArray' t minVal acc

    let nums = List.sort nums
    partitionArray' (List.tail nums) (List.head nums) 1

// 2
partitionArray [ 3; 6; 1; 2; 5 ] 2

// 2
partitionArray [ 1; 2; 3 ] 1
