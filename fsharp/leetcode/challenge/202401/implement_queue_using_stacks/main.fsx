type MyQueue =
    { stack: int list }

    static member init() : MyQueue = { stack = [] }

    static member push (x: int) (q: MyQueue) : MyQueue = { stack = x :: q.stack }

    static member pop(q: MyQueue) : (int * MyQueue) =
        let rec pop' q tmp =
            match q with
            | [] -> failwith "never reach here"
            | h :: [] -> h, { stack = List.rev tmp }
            | h :: t -> pop' t (h :: tmp)

        pop' q.stack []

    static member peek(q: MyQueue) : int =
        let rec peek' q tmp =
            match q with
            | [] -> failwith "never reach here"
            | h :: [] -> h
            | h :: t -> peek' t (h :: tmp)

        peek' q.stack []

    static member empty(q: MyQueue) : bool = List.isEmpty q.stack

let q = MyQueue.init ()
let q1 = MyQueue.push 1 q
let q2 = MyQueue.push 2 q1
let ret1 = MyQueue.peek q2
let ret2, q3 = MyQueue.pop q2
let ret3 = MyQueue.empty q3
let ret4, q4 = MyQueue.pop q3
let ret5 = MyQueue.empty q4
// 1, 1, false, 2, true
ret1, ret2, ret3, ret4, ret5
