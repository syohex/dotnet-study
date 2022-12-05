type List =
    | Leaf
    | Node of int * List

let middleNode (head: List) : List =
    let rec middleNode' node n =
        match node with
        | Leaf -> None, n
        | Node(_, next) ->
            let node', len = middleNode' next (n + 1)

            match node' with
            | Some(_) -> node', len
            | None -> if n = len / 2 then Some(node), len else None, len

    middleNode' head 0 |> fst |> Option.get

let list1 = Node(1, Node(2, Node(3, Node(4, Node(5, Leaf)))))
// [3, 4, 5]
middleNode list1

let list2 = Node(1, Node(2, Node(3, Node(4, Node(5, Node(6, Leaf))))))
// [4, 5, 6]
middleNode list2
