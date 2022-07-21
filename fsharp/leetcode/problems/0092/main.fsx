type List =
    | Leaf
    | Node of int * List

let reverseBetween (head: List) (left: int) (right: int) : List =
    let rec collectNums node i left right acc =
        if i > right then
            acc
        else
            match node with
            | Leaf -> failwith "never reach here"
            | Node (v, next) ->
                if i >= left then
                    collectNums next (i + 1) left right (v :: acc)
                else
                    collectNums next (i + 1) left right acc

    let rec reverseBetween' node i left right nums =
        match nums with
        | [] -> node
        | h :: t ->
            match node with
            | Leaf -> Leaf
            | Node (v, next) ->
                if i >= left then
                    Node(h, reverseBetween' next (i + 1) left right t)
                else
                    Node(v, reverseBetween' next (i + 1) left right nums)

    let nums = collectNums head 1 left right []
    reverseBetween' head 1 left right nums

let list1 =
    Node(1, Node(2, Node(3, Node(4, Node(5, Leaf)))))
// [1;4;3;2;5]
reverseBetween list1 2 4

let list2 =
    Node(1, Node(2, Node(3, Node(4, Node(5, Leaf)))))
// [5;4;3;2;1]
reverseBetween list2 1 5

let list3 = Node(5, Leaf)
// [1]
reverseBetween list3 1 1
