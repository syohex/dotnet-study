open System

let minOperations (nums: int []) (x: int) : int =
    let rec minOperations' (nums: int []) left right sum total x ret =
        if right >= nums.Length then
            if ret = -1 then
                -1
            else
                nums.Length - ret
        else
            let mutable sum' = sum + nums.[right]
            let mutable left' = left

            while left' < right && sum' > total - x do
                sum' <- sum' - nums.[left']
                left' <- left' + 1

            if sum' = total - x then
                let ret' = Math.Max(ret, right - left' + 1)
                minOperations' nums left' (right + 1) sum' total x ret'
            else
                minOperations' nums left' (right + 1) sum' total x ret


    let sum = Array.sum nums
    minOperations' nums 0 0 0 sum x -1

// 2
minOperations [| 1; 1; 4; 2; 3 |] 5

// -1
minOperations [| 5; 6; 7; 8; 9 |] 4

// 5
minOperations [| 3; 2; 20; 1; 1; 3 |] 10
