type ListNode =
    | Leaf
    | Node of int * ListNode

let middleNode (head: ListNode) : ListNode =
    let rec middleNode' node i =
        match node with
        | Leaf -> i, Leaf
        | Node(_, next) ->
            let len, p = middleNode' next (i + 1)

            match p with
            | Leaf -> if i = len / 2 then len, node else len, Leaf
            | _ -> len, p

    middleNode' head 0 |> snd

let list1 = Node(1, Node(2, Node(3, Node(4, Node(5, Leaf)))))
// [3,4,5]
middleNode list1

let list2 = Node(1, Node(2, Node(3, Node(4, Node(5, Node(6, Leaf))))))
// [4,5,6]
middleNode list2
