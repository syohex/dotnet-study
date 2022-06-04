let toNums (num: int) : int list =
    let rec toNums' num acc =
        if num = 0 then
            acc
        else
            toNums' (num / 10) ((num % 10) :: acc)

    if num = 0 then
        [ 0 ]
    else
        toNums' num []

let numsToNum (nums: int []) : int =
    nums |> Array.fold (fun acc n -> acc * 10 + n) 0

let maximumSwap (num: int) : int =
    let rec maximumSwap' pos (nums: int []) =
        if pos >= nums.Length then
            numsToNum nums
        else
            let (maxPos, maxVal) =
                nums
                |> Array.mapi (fun i n -> i, n)
                |> Array.skip (pos + 1)
                |> Array.fold (fun (maxI, max) (i, n) -> if max <= n then i, n else maxI, max) (pos, nums.[pos])

            if maxVal <> nums.[pos] then
                let tmp = nums.[maxPos]
                nums.[maxPos] <- nums.[pos]
                nums.[pos] <- tmp
                numsToNum nums
            else
                maximumSwap' (pos + 1) nums

    let nums = toNums num |> List.toArray
    maximumSwap' 0 nums

// 7236
maximumSwap 2736

// 9973
maximumSwap 9973

// 9913
maximumSwap 1993

// 52341342
maximumSwap 22341345

// 98863
maximumSwap 98368
