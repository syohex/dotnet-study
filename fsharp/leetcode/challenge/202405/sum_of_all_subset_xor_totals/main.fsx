let subsetXORSum (nums: int list) : int =
    let rec subsetXORSum' nums acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let v1 = subsetXORSum' t (acc ^^^ h)
            let v2 = subsetXORSum' t acc
            v1 + v2

    subsetXORSum' nums 0

// 6
subsetXORSum [ 1; 3 ]

// 28
subsetXORSum [ 5; 1; 6 ]

// 480
subsetXORSum [ 3; 4; 5; 6; 7; 8 ]
