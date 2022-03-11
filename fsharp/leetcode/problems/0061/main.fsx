type LinkedList =
    | ListEnd
    | ListNode of int * LinkedList

    static member length(lst: LinkedList) : int =
        let rec length' lst acc =
            match lst with
            | ListEnd -> acc
            | ListNode (_, rest) -> length' rest (acc + 1)

        length' lst 0

    static member take (n: int) (lst: LinkedList) : LinkedList =
        if n = 0 then
            ListEnd
        else
            match lst with
            | ListEnd -> failwith "list is too short"
            | ListNode (v, rest) -> ListNode(v, LinkedList.take (n - 1) rest)

    static member skip (n: int) (lst: LinkedList) : LinkedList =
        if n = 0 then
            lst
        else
            match lst with
            | ListEnd -> failwith "list is too short"
            | ListNode (_, rest) -> LinkedList.skip (n - 1) rest

    static member concat (lst1: LinkedList) (lst2: LinkedList) : LinkedList =
        match lst1 with
        | ListEnd -> lst2
        | ListNode (v1, rest1) -> ListNode(v1, LinkedList.concat rest1 lst2)

    static member toList(lst: LinkedList) : int list =
        match lst with
        | ListEnd -> []
        | ListNode (v, rest) -> v :: (LinkedList.toList rest)

let rotateRight (lst: LinkedList) (k: int) : LinkedList =
    let len = lst |> LinkedList.length

    if len = 0 then
        lst
    else
        let k = k % len
        let lst1 = lst |> LinkedList.skip (len - k)
        let lst2 = lst |> LinkedList.take (len - k)
        LinkedList.concat lst1 lst2

// [4;5;1;2;3]
rotateRight (ListNode(1, ListNode(2, ListNode(3, ListNode(4, ListNode(5, ListEnd)))))) 2
|> LinkedList.toList

// [2;0;1]
rotateRight (ListNode(0, ListNode(1, ListNode(2, ListEnd)))) 4
|> LinkedList.toList

// []
rotateRight ListEnd 4 |> LinkedList.toList

// [1]
rotateRight (ListNode(1, ListEnd)) 0
|> LinkedList.toList

// [1, 2]
rotateRight (ListNode(1, ListNode(2, ListEnd))) 0
|> LinkedList.toList
