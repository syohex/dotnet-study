#r "nuget:FSharpx.Collections"

open FSharpx.Collections

type KthLargest =
    { k: int
      mutable len: int
      mutable q: IPriorityQueue<int> }

    static member popQueue len k queue =
        if len <= k then
            queue, len
        else
            let _, rest = PriorityQueue.pop queue
            KthLargest.popQueue (len - 1) k rest

    static member empty (k: int) (nums: int list) : KthLargest =
        let q =
            nums
            |> List.fold (fun acc n -> PriorityQueue.insert n acc) (PriorityQueue.empty false)

        let len = nums |> List.length
        let q', len' = KthLargest.popQueue len k q

        { k = k; len = len'; q = q' }

    member this.add(value: int) : int =
        this.q <- (PriorityQueue.insert value this.q)

        let q, len =
            KthLargest.popQueue (this.len + 1) this.k this.q

        this.len <- len
        this.q <- q

        PriorityQueue.peek this.q

let mutable k = KthLargest.empty 3 [ 4; 5; 8; 2 ]
// 4
k.add 3
// 5
k.add 5
// 5
k.add 10
// 8
k.add 9
// 8
k.add 4
