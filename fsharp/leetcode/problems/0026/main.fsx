let removeDuplicates (nums: int []) : int =
    let rec removeDuplicates' i prev pos (nums: int []) =
        if i >= nums.Length then
            pos
        else if prev = nums.[i] then
            removeDuplicates' (i + 1) prev pos nums
        else
            nums.[pos] <- nums.[i]
            removeDuplicates' (i + 1) nums.[i] (pos + 1) nums

    removeDuplicates' 1 nums.[0] 1 nums

let nums1 = [| 1; 1; 2 |]
// 2
let ret1 = removeDuplicates nums1
// [1,2]
nums1 |> Array.take ret1

let nums2 = [| 0; 0; 1; 1; 1; 2; 2; 3; 3; 4 |]
// 5
let ret2 = removeDuplicates nums2
// [0,1,2,3,4]
nums2 |> Array.take ret2
