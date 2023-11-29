let hammingWeight (n: uint32) : int =
    let rec hammingWeight' (n: uint32) acc =
        if n = 0u then
            int acc
        else
            hammingWeight' (n >>> 1) (acc + (n &&& 1u))

    hammingWeight' n 0u

// 16
hammingWeight 0xFFFFu

// 3
hammingWeight 0xDu

// 31
hammingWeight 0xFFFFFFF7u
