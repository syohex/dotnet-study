let minBitwiseArray (nums: int list) : int list =
    let rec f n mask =
        if n &&& mask = 0 then
            n &&& ~~~(mask >>> 1)
        else
            f n (mask <<< 1)

    nums |> List.map (fun n -> if n % 2 = 0 then -1 else f n 1)

// [-1,1,4,3]
minBitwiseArray [ 2; 3; 5; 7 ]

// [9,12,15]
minBitwiseArray [ 11; 13; 31 ]
