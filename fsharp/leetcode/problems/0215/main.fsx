#r "nuget:FSharpx.Collections"

open FSharpx.Collections

let findKthLargest (nums: int list) (k: int) : int =
    let rec findKthLargest' q k =
        if k = 1 then
            PriorityQueue.peek q
        else
            let _, q' = PriorityQueue.pop q
            findKthLargest' q' (k - 1)

    let q =
        nums
        |> List.fold (fun q n -> PriorityQueue.insert n q) (PriorityQueue.empty true)

    findKthLargest' q k

// 5
findKthLargest [ 3; 2; 1; 5; 6; 4 ] 2

// 4
findKthLargest [ 3; 2; 3; 1; 2; 4; 5; 5; 6 ] 4
