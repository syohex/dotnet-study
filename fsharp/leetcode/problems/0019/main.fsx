type List =
    | Leaf
    | Node of int * List

let removeNthFromEnd(head: List) (n: int) : List =
    let rec removeNthFromEnd' node pos n =
        match node with
        | Leaf -> pos, Leaf
        | Node(v, next) ->
            let lastPos, next' = removeNthFromEnd' next (pos + 1) n
            if lastPos - pos = n then
                lastPos, next'
            else
                lastPos, Node(v, next')

    removeNthFromEnd' head 0 n |> snd

let list1 = Node(1, Node(2, Node(3, Node(4, Node(5, Leaf)))))
// [1, 2, 3, 5]
removeNthFromEnd list1 2

// []
removeNthFromEnd (Node(1, Leaf)) 1

// [1]
removeNthFromEnd (Node(1, Node(2, Leaf))) 1
