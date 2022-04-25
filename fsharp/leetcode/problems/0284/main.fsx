type Iterator =
    { mutable Nums: int list }

    static member init(nums: int list) : Iterator = { Nums = nums }

    member this.next() : int =
        let ret = List.head this.Nums
        this.Nums <- List.tail this.Nums
        ret

    member this.hasNext() : bool = List.isEmpty this.Nums

type PeekingIterator =
    { mutable Nums: Iterator
      mutable peeked: bool
      mutable value: int }

    static member init(nums: int list) : PeekingIterator =
        { Nums = Iterator.init (nums)
          peeked = false
          value = -1 }

    member this.peek() : int =
        if not this.peeked then
            this.peeked <- true
            this.value <- this.Nums.next ()
            this.value
        else
            this.value

    member this.next() : int =
        if this.peeked then
            this.peeked <- false
            this.value
        else
            this.Nums.next ()

    member this.hasNext() : bool =
        if this.peeked then
            true
        else
            this.Nums.hasNext ()

let pi = PeekingIterator.init [ 1; 2; 3 ]
// 1
pi.next ()
// 2
pi.peek ()
// true
pi.hasNext ()
// 2
pi.next ()
// 3
pi.next ()
// false
pi.hasNext ()
