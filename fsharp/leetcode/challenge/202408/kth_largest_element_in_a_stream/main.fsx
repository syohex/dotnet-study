#r "nuget:FSharpx.Collections"
open FSharpx.Collections

type KthLargest =
    { q: IPriorityQueue<int>
      len: int
      k: int }

    static member init (k: int) (nums: int list) : KthLargest =
        nums
        |> List.fold
            (fun acc n -> snd (KthLargest.add n acc))
            ({ q = PriorityQueue.empty false
               len = 0
               k = k })

    static member add (n: int) (k: KthLargest) : (int * KthLargest) =
        if k.len < k.k then
            let q = PriorityQueue.insert n k.q
            PriorityQueue.peek q, { k with q = q; len = k.len + 1 }
        else if PriorityQueue.peek k.q > n then
            PriorityQueue.peek k.q, k
        else
            let _, q = PriorityQueue.pop k.q
            let q = PriorityQueue.insert n q
            PriorityQueue.peek q, { k with q = q }

let k = KthLargest.init 3 [ 4; 5; 8; 2 ]
// 4
let r1, k1 = KthLargest.add 3 k
// 5
let r2, k2 = KthLargest.add 5 k1
// 5
let r3, k3 = KthLargest.add 10 k2
// 8
let r4, k4 = KthLargest.add 9 k3
// 8
let r5, _ = KthLargest.add 4 k4

printfn "%d, %d, %d, %d, %d" r1 r2 r3 r4 r5
