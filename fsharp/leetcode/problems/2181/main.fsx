type ListNode =
    | ListEnd
    | ListNode of int * ListNode

    static member toList(lst: ListNode) : int list =
        let rec toList' lst acc =
            match lst with
            | ListEnd -> acc |> List.rev
            | ListNode (n, rest) -> toList' rest (n :: acc)

        toList' lst []

let mergeNode (lst: ListNode) : ListNode =
    let rec mergeNode' lst sum =
        match lst with
        | ListEnd -> ListEnd
        | ListNode (n, rest) ->
            if n = 0 then
                ListNode(sum, mergeNode' rest 0)
            else
                mergeNode' rest (sum + n)

    match lst with
    | ListEnd -> failwith "never reach here"
    | ListNode (_, rest) -> mergeNode' rest 0

// [4, 11]
let data1 =
    ListNode(0, ListNode(3, ListNode(1, ListNode(0, ListNode(4, ListNode(5, ListNode(2, ListNode(0, ListEnd))))))))

mergeNode data1 |> ListNode.toList

// [1, 3, 4]
let data2 =
    ListNode(0, ListNode(1, ListNode(0, ListNode(3, ListNode(0, ListNode(2, ListNode(2, ListNode(0, ListEnd))))))))

mergeNode data2 |> ListNode.toList
