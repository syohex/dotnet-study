let maximumBeauty (nums: int[]) (k: int) : int =
    let rec updateWindow left right (nums: int[]) =
        if left > right then
            left
        else if nums.[right] - nums.[left] > 2 * k then
            updateWindow (left + 1) right nums
        else
            left

    let rec maximumBeauty' left right (nums: int[]) acc =
        if right >= nums.Length then
            acc
        else
            let left = updateWindow left right nums
            maximumBeauty' left (right + 1) nums (max acc (right - left + 1))

    maximumBeauty' 0 0 (Array.sort nums) 0

// 3
maximumBeauty [| 4; 6; 1; 2 |] 2

// 4
maximumBeauty [| 1; 1; 1; 1 |] 10
