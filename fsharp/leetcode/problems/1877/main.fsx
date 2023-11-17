open System

let minPairSum (nums: int list) : int =
    let len = List.length nums
    let nums' = List.sort nums

    seq { 0 .. (len - 1) }
    |> Seq.fold
        (fun acc i ->
            let sum = nums'.[i] + nums'.[len - 1 - i]
            Math.Max(acc, sum))
        0

// 7
minPairSum [ 3; 5; 2; 3 ]

// 8
minPairSum [ 3; 5; 4; 2; 4; 6 ]
