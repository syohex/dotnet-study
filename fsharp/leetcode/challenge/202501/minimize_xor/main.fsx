let countOnes (num: int) =
    let rec countOnes' num acc =
        if num = 0 then
            acc
        else
            countOnes' (num >>> 1) (acc + (num &&& 1))

    countOnes' num 0

let setAtOneBit (num: int) (ones: int) : int * int =
    let rec setAtOneBit' num ones mask acc =
        if ones = 0 || mask = 0u then
            int acc, ones
        else if (num &&& mask) <> 0u then
            setAtOneBit' num (ones - 1) (mask >>> 1) (acc ||| mask)
        else
            setAtOneBit' num ones (mask >>> 1) acc

    setAtOneBit' (uint32 num) ones 0x80000000u 0u

let setAtZeroBit (num: int) (ones: int) : int =
    let rec setAtZeroBit' num ones mask acc =
        if ones = 0 then
            acc
        else if (num &&& mask) = 0 then
            setAtZeroBit' num (ones - 1) (mask <<< 1) (acc ||| mask)
        else
            setAtZeroBit' num ones (mask <<< 1) acc

    setAtZeroBit' num ones 1 0

let minimizeXor (num1: int) (num2: int) : int =
    let ones = countOnes num2
    let v1, ones = setAtOneBit num1 ones
    let v2 = setAtZeroBit num1 ones
    v1 ||| v2

// 3
minimizeXor 3 5

// 3
minimizeXor 1 12

// 24
minimizeXor 25 72
