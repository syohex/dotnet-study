let missingNumber (nums: int list) : int =
    let len = nums.Length
    let expected = (1 + len) * len / 2
    let sum = nums |> List.sum
    expected - sum

// 2
missingNumber [ 3; 0; 1 ]

// 2
missingNumber [ 0; 1 ]

// 8
missingNumber [ 9
                6
                4
                2
                3
                5
                7
                0
                1 ]
