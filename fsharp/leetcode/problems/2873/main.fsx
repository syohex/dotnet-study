let maximumTripletValue (nums: int list) : int64 =
    let nums' = nums |> List.map int64 |> List.toArray
    let len = nums'.Length

    let mutable ret = 0L

    for i in 0 .. (len - 3) do
        for j in (i + 1) .. (len - 2) do
            for k in (j + 1) .. (len - 1) do
                ret <- System.Math.Max(ret, (nums'.[i] - nums'.[j]) * nums'.[k])

    ret

// 77
maximumTripletValue [ 12; 6; 1; 2; 7 ]

// 133
maximumTripletValue [ 1; 10; 3; 4; 19 ]

// 0
maximumTripletValue [ 1; 2; 3 ]
