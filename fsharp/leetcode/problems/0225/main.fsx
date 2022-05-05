#r "nuget:FSharpx.Collections"

open FSharpx.Collections

type MyStack =
    { mutable Queue1: Queue<int>
      mutable Queue2: Queue<int>
      mutable Top: int }

    static member init() : MyStack =
        { Queue1 = Queue.empty
          Queue2 = Queue.empty
          Top = -1 }

    member this.push(x: int) =
        this.Queue1 <- Queue.conj x this.Queue1
        this.Top <- x

    member this.pop() : int =
        while (Queue.length this.Queue1) > 1 do
            let v = Queue.head this.Queue1
            this.Queue1 <- Queue.tail this.Queue1
            this.Queue2 <- Queue.conj v this.Queue2
            this.Top <- v

        let ret = Queue.head this.Queue1
        this.Queue1 <- this.Queue2
        this.Queue2 <- Queue.empty

        ret

    member this.top() : int = this.Top

    member this.empty() : bool = Queue.isEmpty this.Queue1

let s = MyStack.init ()
s.push 1
s.push 2
// 2
s.top ()
// 2
s.pop ()
// 1
s.top ()
// false
s.empty ()
// 1
s.pop ()
// true
s.empty ()
