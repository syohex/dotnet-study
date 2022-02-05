type LinkedList<'a when 'a: comparison> =
    | ListEnd
    | ListNode of 'a * LinkedList<'a>

    static member toList(list: LinkedList<'a>) : ('a list) =
        let rec toList' list acc =
            match list with
            | ListEnd -> acc |> List.rev
            | ListNode (v, tail) -> toList' tail (v :: acc)

        toList' list []

let mergeKLists<'a when 'a: comparison> (lists: LinkedList<'a> list) : LinkedList<'a> =
    let rec mergeTwoLists l1 l2 =
        match (l1, l2) with
        | (ListEnd, ListEnd) -> ListEnd
        | (ListNode (n1, t1), ListEnd) -> ListNode(n1, (mergeTwoLists t1 ListEnd))
        | (ListEnd, ListNode (n2, t2)) -> ListNode(n2, (mergeTwoLists ListEnd t2))
        | (ListNode (n1, t1), ListNode (n2, t2)) ->
            if n1 < n2 then
                ListNode(n1, mergeTwoLists t1 l2)
            else
                ListNode(n2, mergeTwoLists l1 t2)

    match lists with
    | [] -> ListEnd
    | head :: tail -> tail |> List.fold mergeTwoLists head


let l1 =
    ListNode(1, ListNode(4, ListNode(5, ListEnd)))

let l2 =
    ListNode(1, ListNode(3, ListNode(4, ListEnd)))

let l3 = ListNode(2, ListNode(6, ListEnd))

// [1,1,2,3,4,4,5,6]
mergeKLists [ l1; l2; l3 ] |> LinkedList.toList

// []
mergeKLists<int> []

// []
mergeKLists<int> [ ListEnd ]
