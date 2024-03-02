open System

let sortedSquare (nums: int[]) : int[] =
    let rec sortedSquare' i (nums: int[]) left right (ret: int[]) =
        if i < 0 then
            ret
        else if Math.Abs(nums.[left]) < Math.Abs(nums.[right]) then
            ret.[i] <- nums.[right] * nums.[right]
            sortedSquare' (i - 1) nums left (right - 1) ret
        else
            ret.[i] <- nums.[left] * nums.[left]
            sortedSquare' (i - 1) nums (left + 1) right ret

    let right = nums.Length - 1
    sortedSquare' right nums 0 right (Array.zeroCreate nums.Length)

// [0,1,9,16,100]
sortedSquare [| -4; -1; 0; 3; 10 |]

// [4,9,9,49,121]
sortedSquare [| -7; -3; 2; 3; 11 |]
