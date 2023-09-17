open System.Numerics

let sumIndicesWithKSetBits (nums: int list) (k: int) : int =
    let rec sumIndicesWithKSetBits' (i: uint32) nums k acc =
        match nums with
        | [] -> acc
        | h :: t ->
            if BitOperations.PopCount(i) = k then
                sumIndicesWithKSetBits' (i + 1u) t k (acc + h)
            else
                sumIndicesWithKSetBits' (i + 1u) t k acc

    sumIndicesWithKSetBits' 0u nums k 0

// 13
sumIndicesWithKSetBits [ 5; 10; 1; 5; 2 ] 1

// 1
sumIndicesWithKSetBits [ 4; 3; 2; 1 ] 2
