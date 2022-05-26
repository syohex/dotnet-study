let hammingWeight (n: uint32) : int =
    let rec hammingWeight' (n: uint32) acc =
        if n = 0u then
            acc
        else
            let acc' = if n &&& 1u = 1u then acc + 1 else acc
            hammingWeight' (n >>> 1) acc'

    hammingWeight' n 0

// 3
hammingWeight 11u

// 1
hammingWeight 256u

// 31
hammingWeight 0xEFFFFFFFu
