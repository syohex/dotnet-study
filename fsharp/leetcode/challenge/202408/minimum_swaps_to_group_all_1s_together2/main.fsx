let minSwaps (nums: int list) : int =
    let minSwaps' nums v =
        let countNum = List.filter ((=) v) >> List.length

        let total = countNum nums
        let maxValInWindow = nums |> List.windowed total |> List.map countNum |> List.max
        total - maxValInWindow

    min (minSwaps' nums 0) (minSwaps' nums 1)

// 1
minSwaps [ 0; 1; 0; 1; 1; 0; 0 ]

// 2
minSwaps [ 0; 1; 1; 1; 0; 0; 1; 1; 0 ]

// 0
minSwaps [ 1; 1; 0; 0; 1 ]
