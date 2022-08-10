type Tree =
    | Leaf
    | Node of int * Tree * Tree

let sortedArrayToBST (nums: int []) : Tree =
    let rec sortedArrayToBST' left right (nums: int []) =
        if left > right then
            Leaf
        else
            let mid = (left + right) / 2
            Node(nums.[mid], sortedArrayToBST' left (mid - 1) nums, sortedArrayToBST' (mid + 1) right nums)

    sortedArrayToBST' 0 (nums.Length - 1) nums

// [0,-3,9,-10,null,5]
sortedArrayToBST [| -10; -3; 0; 5; 9 |]

// [1, 3]
sortedArrayToBST [| 1; 3 |]
