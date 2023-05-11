let maxUncrossedLines (nums1: int[]) (nums2: int[]) : int =
    let len1 = nums1.Length
    let len2 = nums2.Length

    let dp = Array2D.zeroCreate (len1 + 1) (len2 + 1)

    for i in 1..len1 do
        for j in 1..len2 do
            if nums1.[i - 1] = nums2.[j - 1] then
                dp.[i, j] <- 1 + dp.[i - 1, j - 1]
            else
                dp.[i, j] <- System.Math.Max(dp.[i - 1, j], dp.[i, j - 1])

    dp.[len1, len2]

// 2
maxUncrossedLines [| 1; 4; 2 |] [| 1; 2; 4 |]

// 3
maxUncrossedLines [| 2; 5; 1; 2; 5 |] [| 10; 5; 2; 1; 5; 2 |]

// 2
maxUncrossedLines [| 1; 3; 7; 1; 7; 5 |] [| 1; 9; 2; 5; 1 |]
