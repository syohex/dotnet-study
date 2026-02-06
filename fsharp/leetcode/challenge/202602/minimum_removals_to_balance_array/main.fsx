let minRemoval (nums: int list) (k: int) : int =
    let k = int64 k

    let rec adjustWindow (nums: int64[]) left right : int =
        if left >= right then
            printfn "hige"
            left
        elif nums.[left] * k < nums.[right] then
            adjustWindow nums (left + 1) right
        else
            left

    let rec minRemoval' (nums: int64[]) left right acc =
        if right >= nums.Length then
            acc
        else
            let left = adjustWindow nums left right
            let acc = min acc (nums.Length - (right - left + 1))
            minRemoval' nums left (right + 1) acc

    let nums = nums |> List.map int64 |> List.sort |> List.toArray
    minRemoval' nums 0 0 System.Int32.MaxValue

// 1
minRemoval [ 2; 1; 5 ] 2

// 2
minRemoval [ 1; 6; 2; 9 ] 3

// 0
minRemoval [ 4; 6 ] 2

// 1
minRemoval [ 1; 20; 30 ] 2
