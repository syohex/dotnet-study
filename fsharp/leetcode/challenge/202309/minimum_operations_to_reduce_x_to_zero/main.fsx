open System

let minOperations (nums: int list) (x: int) : int =
    let rec adjustWindow (nums: int[]) left right sum diff =
        if left > right || sum <= diff then
            left, sum
        else
            adjustWindow nums (left + 1) right (sum - nums.[left]) diff

    let rec minOperations' (nums: int[]) left right sum diff ret =
        if right >= nums.Length then
            if ret = -1 then -1 else nums.Length - ret
        else
            let sum' = sum + nums.[right]
            let left', sum'' = adjustWindow nums left right sum' diff

            if sum'' = diff then
                minOperations' nums left' (right + 1) sum'' diff (Math.Max(ret, right - left' + 1))
            else
                minOperations' nums left' (right + 1) sum'' diff ret

    let nums' = Array.ofList nums
    let diff = (Array.sum nums') - x
    minOperations' nums' 0 0 0 diff -1

// 2
minOperations [ 1; 1; 4; 2; 3 ] 5

// -1
minOperations [ 5; 6; 7; 8; 9 ] 4

// 5
minOperations [ 3; 2; 20; 1; 1; 3 ] 10
