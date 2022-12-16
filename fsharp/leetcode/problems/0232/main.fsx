type MyQueue =
    { stack1: int list
      stack2: int list }

    static member empty() : MyQueue = { stack1 = []; stack2 = [] }

    static member push (x: int) (q: MyQueue) : MyQueue = { q with stack1 = (x :: q.stack1) }

    static member pop(q: MyQueue) : int * MyQueue =
        let rec pop' stack1 stack2 =
            match stack1 with
            | [] -> [], stack2
            | h :: t -> pop' t (h :: stack2)

        let _, stack2' = pop' q.stack1 q.stack2
        let ret = List.head stack2'
        let stack2'', stack1'' = pop' (stack2' |> List.tail) []

        ret, { stack1 = stack1''; stack2 = stack2'' }

    static member peek(q: MyQueue) : int =
        let rec peek' stack1 stack2 =
            match stack1 with
            | [] -> [], stack2
            | h :: t -> peek' t (h :: stack2)

        let _, stack2' = peek' q.stack1 q.stack2
        List.head stack2'

    static member isEmpty(q: MyQueue) : bool = List.isEmpty q.stack1

let q = MyQueue.empty ()
let q1 = MyQueue.push 1 q |> MyQueue.push 2
// 1
MyQueue.peek q1

let n2, q2 = MyQueue.pop q1
// 1
n2

// false
MyQueue.isEmpty q2
