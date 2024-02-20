let missingNumber (nums: int list) : int =
    let n = List.length nums
    let v = seq { 0 .. (n - 1) } |> Seq.fold (fun acc m -> acc ^^^ m) n
    nums |> List.fold (fun acc m -> acc ^^^ m) v

// 2
missingNumber [ 3; 0; 1 ]

// 2
missingNumber [ 0; 1 ]

// 8
missingNumber [ 9; 6; 4; 2; 3; 5; 7; 0; 1 ]
