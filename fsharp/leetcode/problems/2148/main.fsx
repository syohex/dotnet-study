let countElements (nums: int list) : int =
    let min = nums |> List.min
    let max = nums |> List.max

    nums
    |> List.filter (fun n -> min < n && n < max)
    |> List.length

// 2
countElements [ 11; 7; 2; 15 ]

// 2
countElements [ -3; 3; 3; 90 ]
