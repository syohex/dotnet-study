type ListNode =
    | Leaf
    | Node of int * ListNode

let partition (head: ListNode) (x: int) : ListNode =
    let rec listToListNode nums =
        match nums with
        | [] -> Leaf
        | h :: t -> Node(h, listToListNode t)

    let rec partition' node x acc1 acc2 =
        match node with
        | Leaf -> listToListNode ((List.rev acc1) @ (List.rev acc2))
        | Node(v, next) ->
            if v < x then
                partition' next x (v :: acc1) acc2
            else
                partition' next x acc1 (v :: acc2)

    partition' head x [] []

let list1 = Node(1, Node(4, Node(3, Node(2, Node(5, Node(2, Leaf))))))
// [1,2,2,4,3,5]
partition list1 3

let list2 = Node(2, Node(1, Leaf))
// [1, 2]
partition list2 2
