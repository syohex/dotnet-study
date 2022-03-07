type LinkedList =
    | ListEnd
    | ListNode of int * LinkedList

    static member toList(r: LinkedList) : int list =
        let rec toList' r acc =
            match r with
            | ListEnd -> acc |> List.rev
            | ListNode (v, rest) -> toList' rest (v :: acc)

        toList' r []


let rec mergeTwoLists (list1: LinkedList) (list2: LinkedList) : LinkedList =
    match list1, list2 with
    | ListEnd, ListEnd -> ListEnd
    | _, ListEnd -> list1
    | ListEnd, _ -> list2
    | ListNode (v1, rest1), ListNode (v2, rest2) ->
        if v1 <= v2 then
            ListNode(v1, mergeTwoLists rest1 list2)
        else
            ListNode(v2, mergeTwoLists list1 rest2)

let list1 = ListNode(1, ListNode(2, ListNode(4, ListEnd)))
let list2 = ListNode(1, ListNode(3, ListNode(4, ListEnd)))

// [1;1;2;3;4;4]
mergeTwoLists list1 list2 |> LinkedList.toList

// []
mergeTwoLists ListEnd ListEnd |> LinkedList.toList

// [0]
mergeTwoLists ListEnd (ListNode(0, ListEnd))
|> LinkedList.toList
