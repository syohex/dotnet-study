type LinkedList =
    | Leaf
    | Node of int * LinkedList

    static member ofList(nums: int list) : LinkedList =
        match nums with
        | [] -> Leaf
        | h :: t -> Node(h, LinkedList.ofList t)

let partition (head: LinkedList) (x: int) : LinkedList =
    let rec partition' node x lowers highers =
        match node with
        | Leaf ->
            let nums =
                (lowers |> List.rev) @ (highers |> List.rev)

            LinkedList.ofList nums
        | Node (v, next) ->
            if v < x then
                partition' next x (v :: lowers) highers
            else
                partition' next x lowers (v :: highers)

    partition' head x [] []

let list1 =
    Node(1, Node(4, Node(3, Node(2, Node(5, Node(2, Leaf))))))

// [1;2;2;4;3;5]
partition list1 3

let list2 = Node(2, Node(1, Leaf))
// [1;2]
partition list2 2
