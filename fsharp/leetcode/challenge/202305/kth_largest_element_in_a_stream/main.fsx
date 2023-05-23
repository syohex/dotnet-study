type KthLargest =
    { nums: int list
      k: int }

    static member init (k: int) (nums: int list) : KthLargest =
        let nums' = List.sort nums |> List.rev |> List.take k
        { nums = nums'; k = k }

    static member add (num: int) (k: KthLargest) : (int * KthLargest) =
        let nums' = (num :: k.nums) |> List.sort |> List.rev |> List.take k.k
        List.last nums', { k with nums = nums' }

let k = KthLargest.init 3 [ 4; 5; 8; 2 ]
let n1, k1 = KthLargest.add 3 k
let n2, k2 = KthLargest.add 5 k1
let n3, k3 = KthLargest.add 10 k2
let n4, k4 = KthLargest.add 9 k3
let n5, _ = KthLargest.add 4 k4

// 4, 5, 5, 8, 8
printfn "[%d,%d,%d,%d,%d]" n1 n2 n3 n4 n5
