let getMaximumXor (nums: int list) (maximumBit: int) : int list =
    let rec getMaximumXor' nums xor mask acc =
        match nums with
        | [] -> acc
        | h :: t ->
            let xor = xor ^^^ h
            let answer = ~~~xor &&& mask
            getMaximumXor' t xor mask (answer :: acc)

    let mask = (1 <<< maximumBit) - 1
    getMaximumXor' nums 0 mask []

// [0,3,2,3]
getMaximumXor [ 0; 1; 1; 3 ] 2

// [5;2;6;5]
getMaximumXor [ 2; 3; 4; 7 ] 3

// [4,3,6,4,6,7]
getMaximumXor [ 0; 1; 2; 2; 5; 7 ] 3
