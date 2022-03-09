type LinkedList =
    | ListEnd
    | ListNode of int * LinkedList

let toList (lst: LinkedList) : int list =
    let rec toList' lst acc =
        match lst with
        | ListEnd -> acc |> List.rev
        | ListNode (n, rest) -> toList' rest (n :: acc)

    toList' lst []

let pairSum (head: LinkedList) : int =
    let lst = toList head
    let len = lst |> List.length
    let half = len / 2

    (lst |> List.take half, lst |> List.skip half |> List.rev)
    ||> List.zip
    |> List.map (fun (a, b) -> a + b)
    |> List.fold (fun max sum -> if max < sum then sum else max) 0

// 6
let list1 = ListNode(5, ListNode(4, ListNode(2, ListNode(1, ListEnd))))
pairSum list1

// 7
let list2 = ListNode(4, ListNode(2, ListNode(2, ListNode(3, ListEnd))))
pairSum list2
