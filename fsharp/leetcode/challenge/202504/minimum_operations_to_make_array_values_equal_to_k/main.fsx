let minOperations (nums: int list) (k: int) : int =
    let s = nums |> Set.ofList
    let len = Set.count s
    let v = nums |> Set.ofList |> Set.toList |> List.sort |> List.head

    if v < k then -1
    elif v = k then len - 1
    else len

// 2
minOperations [ 5; 2; 5; 4; 5 ] 2

// -1
minOperations [ 2; 1; 2 ] 2

// 4
minOperations [ 9; 7; 5; 3 ] 1
