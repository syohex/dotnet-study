let minOperations (grid: int[,]) (x: int) : int =
    let rec minOperations' i (nums: int[]) mid acc =
        if i >= nums.Length then
            acc
        else
            let diff = abs (mid - nums.[i])

            if diff % x <> 0 then
                -1
            else
                minOperations' (i + 1) nums mid (acc + diff / x)

    let nums = grid |> Seq.cast<int> |> Seq.sort |> Seq.toArray
    let mid = nums.[nums.Length / 2]
    minOperations' 0 nums mid 0

let grid1 = array2D [ [ 2; 4 ]; [ 6; 8 ] ]
// 4
minOperations grid1 2

let grid2 = array2D [ [ 1; 5 ]; [ 2; 3 ] ]
// 5
minOperations grid2 1

let grid3 = array2D [ [ 1; 2 ]; [ 3; 4 ] ]
// -1
minOperations grid3 2
