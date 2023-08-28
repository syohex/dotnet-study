type MyStack =
    { queue: int list }

    static member empty() : MyStack = { queue = [] }

    static member push (x: int) (s: MyStack) : MyStack = { s with queue = s.queue @ [ x ] }

    static member pop(s: MyStack) : (int * MyStack) =
        let rec pop' q acc =
            match q with
            | [] -> failwith "never reach here"
            | h :: [] -> h, { queue = List.rev acc }
            | h :: t -> pop' t (h :: acc)

        pop' s.queue []

    static member top(s: MyStack) : int =
        let rec top' q =
            match q with
            | [] -> failwith "never reach here"
            | h :: [] -> h
            | _ :: t -> top' t

        top' s.queue

    static member empty(s: MyStack) : bool = List.isEmpty s.queue

let m = MyStack.empty ()
let m1 = MyStack.push 1 m
let m2 = MyStack.push 2 m1

// 2
MyStack.top m2

let n3, m3 = MyStack.pop m2
// 2, false
printfn "pop=%d isEmpty=%b" n3 (MyStack.empty m3)

let n4, m4 = MyStack.pop m3
// 1, true
printfn "pop=%d isEmpty=%b" n4 (MyStack.empty m4)
