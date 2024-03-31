let minOperations (nums: int list) (k: int) =
    nums |> List.sort |> List.findIndex (fun num -> num >= k)

// 3
minOperations [ 2; 11; 10; 1; 3 ] 10

// 0
minOperations [ 1; 1; 2; 4; 9 ] 1

// 4
minOperations [ 1; 1; 2; 4; 9 ] 9
