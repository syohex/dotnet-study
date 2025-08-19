let zeroFilledSubarray (nums: int list) : int64 =
    let rec zeroFilledSubarray' nums (zeros: int64) acc =
        match nums with
        | [] -> acc
        | h :: t ->
            if h = 0 then
                let zeros = zeros + 1L
                zeroFilledSubarray' t zeros (acc + zeros)
            else
                zeroFilledSubarray' t 0 acc

    zeroFilledSubarray' nums 0L 0L

// 6
zeroFilledSubarray [ 1; 3; 0; 0; 2; 0; 0; 4 ]

// 9
zeroFilledSubarray [ 0; 0; 0; 2; 0; 0 ]

// 0
zeroFilledSubarray [ 2; 10; 2019 ]
