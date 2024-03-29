let countSubarrays (nums: int list) (k: int) : int =
    let rec moveLeft (nums: int[]) left max count =
        if count < k || left >= nums.Length then
            left, count
        else if nums.[left] = max then
            moveLeft nums (left + 1) max (count - 1)
        else
            moveLeft nums (left + 1) max count

    let rec countSubarrays' (nums: int[]) left right count max acc =
        if right >= nums.Length then
            acc
        else
            let count' = if nums.[right] = max then count + 1 else count
            let left', count' = moveLeft nums left max count'
            countSubarrays' nums left' (right + 1) count' max (acc + left')

    let max = List.max nums
    countSubarrays' (List.toArray nums) 0 0 0 max 0

// 6
countSubarrays [ 1; 3; 2; 3; 3 ] 2

// 0
countSubarrays [ 1; 4; 2; 1 ] 3
