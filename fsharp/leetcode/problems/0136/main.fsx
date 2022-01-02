let singleNumber(nums: int list) : int =
    let rec singleNumber' (nums: int list) (num: int) : int =
        match nums with
        | [] -> num
        | n :: tail -> singleNumber' tail (num ^^^ n)

    singleNumber' nums 0

singleNumber [2;2;1]
singleNumber [4;1;2;1;2]
singleNumber [1]
