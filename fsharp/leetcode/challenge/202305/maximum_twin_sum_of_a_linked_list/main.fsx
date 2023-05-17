type ListNode =
    | Leaf
    | Node of int * ListNode

let pairSum (head: ListNode) : int =
    let rec listNodeToList node acc =
        match node with
        | Leaf -> List.rev acc
        | Node(v, next) -> listNodeToList next (v :: acc)

    let lst = listNodeToList head []
    let len = List.length lst
    let front, back = List.take (len / 2) lst, List.skip (len / 2) lst

    List.zip front (List.rev back) |> List.map (fun (a, b) -> a + b) |> List.max


let list1 = Node(5, Node(4, Node(2, Node(1, Leaf))))
// 6
pairSum list1

let list2 = Node(4, Node(2, Node(2, Node(3, Leaf))))
// 7
pairSum list2

let list3 = Node(1, Node(100000, Leaf))
// 100001
pairSum list3
