type LinkedList =
    | ListEnd
    | ListNode of int * LinkedList

    static member toList (lst: LinkedList) : int list =
        let rec toList' lst acc =
            match lst with
            | ListEnd -> acc |> List.rev
            | ListNode(v, rest) -> toList' rest (v :: acc)

        toList' lst []

let countDuplicateNode (head: LinkedList) : (int * int) list =
    let rec countDuplicateNode' head prev count acc =
        match head with
        | ListEnd -> ((prev, count) :: acc) |> List.rev
        | ListNode (v, rest) ->
            if v = prev then
                countDuplicateNode' rest prev (count + 1) acc
            else
                countDuplicateNode' rest v 1 ((prev, count) :: acc)

    match head with
    | ListEnd -> []
    | ListNode (v, rest) -> countDuplicateNode' rest v 1 []

let deleteDuplicates (head: LinkedList) : LinkedList =
    let rec deleteDuplicates' notDups =
        match notDups with
        | [] -> ListEnd
        | (v, _) :: rest -> ListNode(v, deleteDuplicates' rest)

    let nonDups =
        countDuplicateNode head
        |> List.filter (fun (_, count) -> count = 1)

    deleteDuplicates' nonDups

// [1;2;5]
let list1 =
    ListNode(1, ListNode(2, ListNode(3, ListNode(3, ListNode(4, ListNode(4, ListNode(5, ListEnd)))))))

deleteDuplicates list1 |> LinkedList.toList

// [2;3]
let list2 =
    ListNode(1, ListNode(1, ListNode(1, ListNode(2, ListNode(3, ListEnd)))))

deleteDuplicates list2 |> LinkedList.toList

// []
let list3 =
    ListNode(1, ListNode(1, ListNode(1, ListNode(1, ListNode(1, ListEnd)))))
deleteDuplicates list3 |> LinkedList.toList

// [2]
let list4 =
    ListNode(1, ListNode(1, ListNode(1, ListNode(1, ListNode(2, ListEnd)))))
deleteDuplicates list4 |> LinkedList.toList

// [1]
let list5 =
    ListNode(1, ListNode(2, ListNode(2, ListNode(2, ListNode(2, ListEnd)))))
deleteDuplicates list5 |> LinkedList.toList