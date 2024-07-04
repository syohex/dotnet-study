type ListNode =
    | Leaf
    | Node of int * ListNode

let mergeNodes (head: ListNode) : ListNode =
    let rec mergeNodes' node acc =
        match node with
        | Leaf -> Leaf
        | Node(v, next) ->
            if v = 0 then
                Node(acc, mergeNodes' next 0)
            else
                mergeNodes' next (acc + v)

    match head with
    | Leaf
    | Node(_, Leaf) -> failwith "never reach here"
    | Node(_, next) -> mergeNodes' next 0

let list1 =
    Node(0, Node(3, Node(1, Node(0, Node(4, Node(5, Node(2, Node(0, Leaf))))))))
//  [4, 11]
mergeNodes list1

let list2 =
    Node(0, Node(1, Node(0, Node(3, Node(0, Node(2, Node(2, Node(0, Leaf))))))))
// [1, 3, 4]
mergeNodes list2
