type ListNode =
    | Leaf
    | Node of int * ListNode

let rec toListAppend (nums: int list) (tail: ListNode) : ListNode =
    match nums with
    | [] -> tail
    | h :: t -> Node(h, toListAppend t tail)

let reverseSubList (node: ListNode) (n: int) : ListNode =
    let rec reverseSubList' node n acc =
        if n = 0 then
            toListAppend acc node
        else
            match node with
            | Leaf -> failwith "never reach here"
            | Node(v, next) -> reverseSubList' next (n - 1) (v :: acc)

    reverseSubList' node n []

let rec reverseBetween (head: ListNode) (left: int) (right: int) : ListNode =
    if left = 1 then
        reverseSubList head right
    else
        match head with
        | Leaf -> failwith "never reach here"
        | Node(v, next) -> Node(v, reverseBetween next (left - 1) (right - 1))

let list1 = Node(1, Node(2, Node(3, Node(4, Node(5, Leaf)))))
// [1,4,3,2,5]
reverseBetween list1 2 4

let list2 = Node(5, Leaf)
// [5]
reverseBetween list2 1 1
