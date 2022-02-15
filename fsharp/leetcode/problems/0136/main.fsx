let singleNumber (nums: int list) : int =
    nums |> List.reduce (fun a b -> a ^^^ b)

// 1
singleNumber [ 2; 2; 1 ]

// 4
singleNumber [ 4; 1; 2; 1; 2 ]

// 1
singleNumber [ 1 ]
