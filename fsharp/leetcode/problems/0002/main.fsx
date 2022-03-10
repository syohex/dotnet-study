type LinkedList =
    | ListEnd
    | ListNode of int * LinkedList

    static member toList lst =
        let rec toList' lst acc =
            match lst with
            | ListEnd -> acc |> List.rev
            | ListNode (v, rest) -> toList' rest (v :: acc)

        toList' lst []

    static member toNumber lst =
        lst
        |> LinkedList.toList
        |> List.fold (fun acc n -> acc * 10 + n) 0

let addTwoNumbers (l1: LinkedList) (l2: LinkedList) : LinkedList =
    let rec addTwoNumbers' l1 l2 carry =
        match l1, l2 with
        | ListEnd, ListEnd ->
            if carry <> 0 then
                ListNode(carry, ListEnd)
            else
                ListEnd
        | ListNode (v1, rest1), ListEnd ->
            let sum = v1 + carry
            ListNode(sum % 10, addTwoNumbers' rest1 l2 (sum / 10))
        | ListEnd, ListNode (v2, rest2) ->
            let sum = v2 + carry
            ListNode(sum % 10, addTwoNumbers' l1 rest2 (sum / 10))
        | ListNode (v1, rest1), ListNode (v2, rest2) ->
            let sum = v1 + v2 + carry
            ListNode(sum % 10, addTwoNumbers' rest1 rest2 (sum / 10))

    addTwoNumbers' l1 l2 0

// [7, 0, 8]
addTwoNumbers (ListNode(2, ListNode(4, ListNode(3, ListEnd)))) (ListNode(5, ListNode(6, ListNode(4, ListEnd))))
|> LinkedList.toNumber

// [0]
addTwoNumbers (ListNode(0, ListEnd)) (ListNode(0, ListEnd))
|> LinkedList.toNumber

// [8, 9, 9, 9, 0, 0, 0, 1]
addTwoNumbers
    (ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListEnd))))))))
    (ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListEnd)))))
|> LinkedList.toNumber
