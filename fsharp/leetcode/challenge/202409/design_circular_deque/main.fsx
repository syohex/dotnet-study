type MyCircularDeque =
    { Q: int[]
      Start: int
      End: int
      Capacity: int
      Size: int }

    static member Init(k: int) : MyCircularDeque =
        { Q = Array.zeroCreate k
          Start = 0
          End = k - 1
          Capacity = k
          Size = 0 }

    static member InsertFront (v: int) (q: MyCircularDeque) : bool * MyCircularDeque =
        if MyCircularDeque.IsFull q then
            false, q
        else
            let start = (q.Start - 1 + q.Capacity) % q.Capacity
            q.Q.[start] <- v

            true,
            { q with
                Q = q.Q
                Start = start
                Size = q.Size + 1 }

    static member InsertLast (v: int) (q: MyCircularDeque) : bool * MyCircularDeque =
        if MyCircularDeque.IsFull q then
            false, q
        else
            let end' = (q.End + 1) % q.Capacity
            q.Q.[end'] <- v

            true,
            { q with
                Q = q.Q
                End = end'
                Size = q.Size + 1 }

    static member DeleteFront(q: MyCircularDeque) : bool * MyCircularDeque =
        if MyCircularDeque.IsEmpty q then
            false, q
        else
            let start = (q.Start + 1) % q.Capacity

            true,
            { q with
                Start = start
                Size = q.Size - 1 }

    static member DeleteLast(q: MyCircularDeque) : bool * MyCircularDeque =
        if MyCircularDeque.IsEmpty q then
            false, q
        else
            let end' = (q.End - 1 + q.Capacity) + q.Capacity
            true, { q with End = end'; Size = q.Size - 1 }

    static member GetFront(q: MyCircularDeque) : int =
        if MyCircularDeque.IsEmpty q then -1 else q.Q.[q.Start]

    static member GetRear(q: MyCircularDeque) : int =
        if MyCircularDeque.IsEmpty q then -1 else q.Q.[q.End]

    static member IsEmpty(q: MyCircularDeque) : bool = q.Size = 0
    static member IsFull(q: MyCircularDeque) : bool = q.Size = q.Capacity

let q = MyCircularDeque.Init 3
let ok1, q1 = MyCircularDeque.InsertLast 1 q
let ok2, q2 = MyCircularDeque.InsertLast 2 q1
let ok3, q3 = MyCircularDeque.InsertFront 3 q2
let ok4, q4 = MyCircularDeque.InsertFront 4 q3
let v1 = MyCircularDeque.GetRear q4
let ok5 = MyCircularDeque.IsFull q4

let ok6, q6 = MyCircularDeque.DeleteLast q4
let ok7, q7 = MyCircularDeque.InsertFront 4 q6
let v2 = MyCircularDeque.GetFront q7

// [true, true, true, false, 2, true, true, true, 4]
ok1, ok2, ok3, ok4, v1, ok5, ok6, ok7, v2
