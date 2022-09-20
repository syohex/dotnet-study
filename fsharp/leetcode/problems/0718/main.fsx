let findLength (nums1: int list) (nums2: int list) : int =
    let nums1' = nums1 |> List.toArray
    let nums2' = nums2 |> List.toArray

    let len1 = nums1'.Length
    let len2 = nums2'.Length

    let dp = Array2D.zeroCreate (len1 + 1) (len2 + 1)
    let mutable ret = 0

    for i in (len1 - 1) .. -1 .. 0 do
        for j in (len2 - 1) .. -1 .. 0 do
            if nums1'.[i] = nums2'.[j] then
                dp.[i, j] <- dp.[i + 1, j + 1] + 1
                ret <- System.Math.Max(ret, dp.[i, j])

    ret

// 3
findLength [ 1; 2; 3; 2; 1 ] [
    3
    2
    1
    4
    7
]

// 5
findLength [ 0; 0; 0; 0; 0 ] [
    0
    0
    0
    0
    0
]
