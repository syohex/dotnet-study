let subsetXorSum (nums: int list) : int =
    let rec subsetXorSum' nums acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let ret1 = subsetXorSum' t (acc ^^^ h)
            let ret2 = subsetXorSum' t acc
            ret1 + ret2

    subsetXorSum' nums 0

// 6
subsetXorSum [ 1; 3 ]

// 28
subsetXorSum [ 5; 1; 6 ]

// 480
subsetXorSum [ 3; 4; 5; 6; 7; 8 ]
