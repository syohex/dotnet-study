let peakIndexInMountainArray (arr: int list) : int =
    let rec peakIndexInMountainArray' (arr: int[]) left right =
        if left >= right then
            left
        else
            let mid = left + (right - left) / 2

            if arr.[mid] < arr.[mid + 1] then
                peakIndexInMountainArray' arr (mid + 1) right
            else
                peakIndexInMountainArray' arr left mid

    let arr' = Array.ofList arr
    peakIndexInMountainArray' arr' 0 (arr.Length - 1)

// 1
peakIndexInMountainArray [ 0; 1; 0 ]

// 1
peakIndexInMountainArray [ 0; 2; 1; 0 ]

// 1
peakIndexInMountainArray [ 0; 10; 5; 2 ]
