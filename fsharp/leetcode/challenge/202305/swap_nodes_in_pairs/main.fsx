type ListNode =
    | Leaf
    | Node of int * ListNode

let swapPair (head: ListNode) =
    let rec swapPair' node =
        match node with
        | Leaf -> Leaf
        | Node(v1, next1) ->
            match next1 with
            | Leaf -> node
            | Node(v2, next2) -> Node(v2, Node(v1, swapPair' next2))

    swapPair' head

let list1 = Node(1, Node(2, Node(3, Node(4, Leaf))))
// [2, 1, 4, 3]
swapPair list1

let list2 = Leaf
// []
swapPair list2

let list3 = Node(1, Leaf)
// [1]
swapPair list3

let list4 = Node(1, Node(2, Node(3, Leaf)))
// [2, 1, 3]
swapPair list4
