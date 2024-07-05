type ListNode =
    | Leaf
    | Node of int * ListNode

let nodesBetweenCriticalPoints (head: ListNode) : (int * int) =
    let rec nodesBetweenCriticalPoints' node i firstCritical prevCritical minDistance =
        match node with
        | Leaf
        | Node(_, Leaf)
        | Node(_, Node(_, Leaf)) ->
            if firstCritical = prevCritical then
                -1, -1
            else
                minDistance, prevCritical - firstCritical
        | Node(prev, current) ->
            match current with
            | Leaf
            | Node(_, Leaf) -> failwith "never reach here"
            | Node(v, Node(next, _)) ->
                let isMaxima = prev < v && v > next
                let isMinima = prev > v && v < next

                if isMaxima || isMinima then
                    if firstCritical = 0 then
                        nodesBetweenCriticalPoints' current (i + 1) i i minDistance
                    else
                        nodesBetweenCriticalPoints' current (i + 1) firstCritical i (min minDistance (i - prevCritical))
                else
                    nodesBetweenCriticalPoints' current (i + 1) firstCritical prevCritical minDistance

    nodesBetweenCriticalPoints' head 1 0 0 System.Int32.MaxValue

let list1 = Node(3, Node(1, Leaf))
// -1, -1
nodesBetweenCriticalPoints list1

let list2 = Node(5, Node(3, Node(1, Node(2, Node(5, Node(1, Node(2, Leaf)))))))
// 1, 3
nodesBetweenCriticalPoints list2

let list3 =
    Node(1, Node(3, Node(2, Node(2, Node(3, Node(2, Node(2, Node(7, Leaf))))))))
// 3, 3
nodesBetweenCriticalPoints list3
