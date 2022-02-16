type LinkedList =
    | Empty
    | ListNode of int * LinkedList

let rec swapPair (node: LinkedList) : LinkedList =
    match node with
    | Empty -> Empty
    | ListNode (x, next) ->
        match next with
        | Empty -> node
        | ListNode (y, nnext) -> ListNode(y, ListNode(x, (swapPair nnext)))

// [2;1;4;3]
swapPair (ListNode(1, ListNode(2, ListNode(3, ListNode(4, Empty)))))

// [2;1;3]
swapPair (ListNode(1, ListNode(2, ListNode(3, Empty))))

// [2;1]
swapPair (ListNode(1, ListNode(2, Empty)))

// [1]
swapPair (ListNode(1, Empty))

// []
swapPair Empty
