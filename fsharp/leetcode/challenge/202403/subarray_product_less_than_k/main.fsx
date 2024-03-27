let numSubarrayProductLessThanK (nums: int list) (k: int) : int =
    let rec moveLeft (nums: int[]) left right product =
        if left >= right || product < k then
            left, product
        else
            moveLeft nums (left + 1) right (product / nums.[left])

    let rec numSubarrayProductLessThanK' (nums: int[]) left right product acc =
        if right >= nums.Length then
            acc
        else
            let product' = product * nums.[right]
            let left', product'' = moveLeft nums left right product'
            let acc' = if product'' < k then acc + right - left' + 1 else acc
            numSubarrayProductLessThanK' nums left' (right + 1) product'' acc'

    numSubarrayProductLessThanK' (List.toArray nums) 0 0 1 0

// 8
numSubarrayProductLessThanK [ 10; 5; 2; 6 ] 100

// 0
numSubarrayProductLessThanK [ 1; 2; 3 ] 0

// 0
numSubarrayProductLessThanK [ 1; 2; 3 ] 1
