let countSubarrays (nums: int list) : int =
    let rec countSubarrays' (nums: double list) acc =
        match nums with
        | []
        | _ :: []
        | _ :: _ :: [] -> acc
        | h1 :: h2 :: h3 :: _ ->
            if h1 + h3 = h2 / 2.0 then
                countSubarrays' (List.tail nums) (acc + 1)
            else
                countSubarrays' (List.tail nums) acc

    countSubarrays' (List.map double nums) 0

// 1
countSubarrays [ 1; 2; 1; 4; 1 ]

// 0
countSubarrays [ 1; 1; 1 ]

// 1
countSubarrays [ -1; -4; -1; 4 ]
